defmodule Technician do
  use GenServer

  @hospital_exchange "hospital"
  @admin_exchange "admin"
  @info_key "info"
  @log_key "log"

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

        {:ok, %{chan: chan}}

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

  def handle_info({:basic_deliver, payload, meta}, state) do
    IO.puts("Diagnosing #{payload}")
    spawn fn -> consume(payload, meta, state) end
    {:noreply, state}
  end

  defp setup(conn) do
    {:ok, chan} = AMQP.Channel.open(conn)
    :ok = AMQP.Exchange.direct(chan, @hospital_exchange, durable: true)
    :ok = AMQP.Basic.qos(chan, prefetch_count: 1)

    IO.puts("Enter specializations")
    input = IO.read(:stdio, :line)
      |> String.split(~r"[^a-zA-Z]")
      |> Enum.filter(fn a -> a != "" end)
    specs = if length(input) > 0 do input else ["knee", "elbow"] end

    Enum.each(
      specs,
      fn spec ->
        queue_name = "#{spec}_queue"
        {:ok, _} = AMQP.Queue.declare(chan, queue_name, auto_delete: true)
        :ok = AMQP.Queue.bind(chan, queue_name, @hospital_exchange, routing_key: spec)
        {:ok, _} = AMQP.Basic.consume(chan, queue_name)
      end
    )

    {:ok, info_queue} = AMQP.Queue.declare(chan)
    :ok = AMQP.Queue.bind(chan, info_queue.queue, @admin_exchange, routing_key: @info_key)
    {:ok, _} = AMQP.Basic.consume(chan, info_queue.queue)

    chan
  end

  defp consume(payload, meta=%{exchange: @admin_exchange}, state) do
    IO.puts("Received info: #{payload}")
    AMQP.Basic.ack(state.chan, meta.delivery_tag)
  end

  defp consume(payload, meta=%{exchange: @hospital_exchange}, state) do
    response = "#{payload} #{meta.routing_key} done"
    :rand.uniform(4) |> Kernel.*(1000) |> :timer.sleep
    IO.puts response
    AMQP.Basic.ack(state.chan, meta.delivery_tag)

    publish_and_log(state.chan, meta.reply_to, response)
  rescue
    exception ->
      Apex.ap({"Error consuming message", exception})
      :ok = AMQP.Basic.reject(state.chan, meta.delivery_tag)
  end

  defp publish_and_log(chan, reply_to, response) do
    AMQP.Basic.publish(chan, @hospital_exchange, reply_to, response)
    AMQP.Basic.publish(chan, @admin_exchange, @log_key, response)
  end
end
