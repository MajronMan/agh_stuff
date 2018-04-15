defmodule Doctor do
  @hospital_exchange "hospital"
  @admin_exchange "admin"
  @info_key "info"
  @log_key "log"

  use GenServer

  def start_link do
    GenServer.start_link(__MODULE__, [], [])
  end

  def init(_opts) do
    rabbitmq_connect()
  end

  defp unique_id do
    :erlang.timestamp
      |> Tuple.to_list
      |> List.insert_at(0, :rand.uniform(1000))
      |> Enum.map(&:erlang.integer_to_binary/1)
      |> Enum.join("")
      |> Base.encode64
  end

  defp rabbitmq_connect do
    case AMQP.Connection.open("amqp://guest:guest@localhost") do
      {:ok, conn} ->
        # Get notifications when the connection goes down
        Process.monitor(conn.pid)
        id = unique_id()
        chan = setup(conn, id)
        schedule_input()

        {:ok, %{chan: chan, id: id}}

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

  def handle_info(:input, state) do
    IO.puts("Enter the name of the patient:")
    name = IO.read(:stdio, :line) |> String.trim()
    IO.puts("Enter the type of diagnosis:")
    type = IO.read(:stdio, :line) |> String.trim()
    publish_and_log(state.chan, type, name, state.id)
    {:noreply, state}
  end

  def handle_info({:basic_deliver, payload, meta}, state) do
      spawn fn -> consume(payload, meta, state) end
      {:noreply, state}
  end

  defp consume(payload, meta=%{exchange: @admin_exchange}, state) do
    IO.puts("Received info: #{payload}")
    AMQP.Basic.ack(state.chan, meta.delivery_tag)
  end

  defp consume(payload, meta=%{exchange: @hospital_exchange}, state) do
    IO.puts("Received results: #{payload}")
    AMQP.Basic.ack(state.chan, meta.delivery_tag)
  end

  defp queue_name(id), do: "#{id}_queue"

  defp setup(conn, id) do
    response_queue = queue_name(id)
    {:ok, chan} = AMQP.Channel.open(conn)
    {:ok, _} = AMQP.Queue.declare(chan, response_queue)
    AMQP.Basic.qos(chan, prefetch_count: 1)

    :ok = AMQP.Exchange.direct(chan, @hospital_exchange, durable: true)
    :ok = AMQP.Queue.bind(chan, response_queue, @hospital_exchange, routing_key: id)
    AMQP.Basic.consume(chan, response_queue)

    {:ok, info_queue} = AMQP.Queue.declare(chan)
    :ok = AMQP.Queue.bind(chan, info_queue.queue, @admin_exchange, routing_key: @info_key)
    {:ok, _} = AMQP.Basic.consume(chan, info_queue.queue)

    chan
  end

  defp publish_and_log(chan, key, request, id) do
    :ok = AMQP.Basic.publish(
      chan,
      @hospital_exchange,
      key,
      request,
      reply_to: id
    )
    :ok = AMQP.Basic.publish(chan, @admin_exchange, @log_key, request)
  end

  defp schedule_input() do
    Process.send_after(self(), :input, 100)
  end

  def request(pid) do
    Process.send_after(pid, :input, 1000)
  end
end

