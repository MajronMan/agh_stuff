package Bank;

import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import sr.grpc.*;

import java.util.*;
import java.util.concurrent.TimeUnit;

public class CurrencyClient {
    private final ManagedChannel channel;
    private final ExchangeGrpc.ExchangeBlockingStub exchangeStub;
    private final  List<CurrencyNameG> currencies = new LinkedList<>();

    private final Map<CurrencyNameG, Double> rates = new HashMap<>();

    public CurrencyClient(String host, int port, Set<CurrencyNameG> currencies) {
        channel = ManagedChannelBuilder.forAddress(host, port)
                .usePlaintext()
                .build();

        exchangeStub = ExchangeGrpc.newBlockingStub(channel);

        RateStateRequest message = RateStateRequest.newBuilder().addAllCurrencies(currencies).build();
        RateStateResponse state = exchangeStub.currentRates(message);

        state.getRatesList()
                .forEach(
                        rate -> rates.put(rate.getName(), rate.getRate())
                );

        this.currencies.addAll(currencies);
        new Thread(this::updateRates).start();
    }

    private void updateRates() {
        RateStateRequest request = RateStateRequest.newBuilder()
                .addAllCurrencies(currencies)
                .build();

        exchangeStub.streamRates(request).forEachRemaining(rate -> {
            synchronized (rates) {
                System.out.println("UPDATING " + rate.getName() + " to " + rate.getRate());
                rates.put(rate.getName(), rate.getRate());
            }
        });
    }


    public void shutdown() throws InterruptedException {
        channel.shutdown().awaitTermination(5, TimeUnit.SECONDS);
    }

    public Map<CurrencyNameG, Double> getRates() {
        return rates;
    }
}
