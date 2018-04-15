defmodule Admin do
  use GenServer

  @admin_exchange "admin"
  @log_queue "log_queue"
  @log_key "log"
  @info_key "info"

  def start_link do
    GenServer.start_link(__MODULE__, [], [])
  end

  def init(_opts) do
    rabbitmq_connect()
  end

  defp rabbitmq_connect do
    case AMQP.Connection.open("amqp://guest:guest@localhost") do
      {:ok, conn} ->
        # Get notifications when the connection goes down
        Process.monitor(conn.pid)
        chan = setup(conn)

        {:ok, %{chan: chan, count: 1}}

      {:error, _} ->
        # Reconnection loop
        :timer.sleep(10000)
        rabbitmq_connect()
    end
  end

  # 2. Implement a callback to handle DOWN notifications from the system
  #    This callback should try to reconnect to the server

  def handle_info({:DOWN, _, :process, _pid, _reason}, state) do
    {:ok, chan} = rabbitmq_connect()
    {:noreply, Map.replace!(state, :chan, chan)}
  end

  # Confirmation sent by the broker after registering this process as a consumer
  def handle_info({:basic_consume_ok, %{consumer_tag: _consumer_tag}}, state) do
    {:noreply, state}
  end

  # Sent by the broker when the consumer is unexpectedly cancelled (such as after a queue deletion)
  def handle_info({:basic_cancel, %{consumer_tag: _consumer_tag}}, state) do
    {:stop, :normal, state}
  end

  # Confirmation sent by the broker to the consumer process after a Basic.cancel
  def handle_info({:basic_cancel_ok, %{consumer_tag: _consumer_tag}}, state) do
    {:noreply, state}
  end

  def handle_info(:send_info, state) do
    info = IO.read(:stdio, :line) |> String.trim()
    :ok = AMQP.Basic.publish(state.chan, @admin_exchange, @info_key, info)
    {:noreply, state}
  end

  def handle_info({:basic_deliver, payload, meta}, state) do
    spawn fn -> consume(payload, meta, state) end
    state = Map.replace!(state, :count, state.count + 1)
    {:noreply, state}
  end

  defp setup(conn) do
    {:ok, chan} = AMQP.Channel.open(conn)
    :ok = AMQP.Exchange.topic(chan, @admin_exchange, durable: true)
    :ok = AMQP.Basic.qos(chan, prefetch_count: 1)

    {:ok, _} = AMQP.Queue.declare(chan, @log_queue, auto_delete: true)
    :ok = AMQP.Queue.bind(chan, @log_queue, @admin_exchange, routing_key: @log_key)
    {:ok, _} = AMQP.Basic.consume(chan, @log_queue)

    chan
  end

  defp consume(payload, meta, state) do
    IO.puts("#{state.count}> #{payload}")
    AMQP.Basic.ack(state.chan, meta.delivery_tag)
  rescue
    exception ->
      Apex.ap({"Error consuming message", exception})
      :ok = AMQP.Basic.reject(state.chan, meta.delivery_tag)
  end

  def send_info(pid) do
    Process.send_after(pid, :send_info, 1000)
  end
end
