package currencyService;


import io.grpc.Server;
import io.grpc.ServerBuilder;

import java.io.IOException;
import java.util.Scanner;

public class CurrencyServer {
    private Server server;

    private void start(int port) throws IOException {
        server = ServerBuilder.forPort(port)
                .addService(new ExchangeImpl())
                .build()
                .start();
        System.out.println("CurrencyServer started on " + port);
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            // Use stderr here since the logger may have been reset by its JVM shutdown hook.
            System.err.println("*** shutting down gRPC server since JVM is shutting down");
            CurrencyServer.this.stop();
            System.err.println("*** server shut down");
        }));
    }

    private void stop() {
        if (server != null) {
            server.shutdown();
        }
    }

    private void blockUntilShutdown() throws InterruptedException {
        if (server != null) {
            server.awaitTermination();
        }
    }

    public static void main(String[] args) throws IOException, InterruptedException {
        final CurrencyServer server = new CurrencyServer();
        System.out.println("Enter server port");
        Scanner s = new Scanner(System.in);
        server.start(s.nextInt());
        server.blockUntilShutdown();
    }
}
