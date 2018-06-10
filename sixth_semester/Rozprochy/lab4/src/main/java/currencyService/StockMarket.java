package currencyService;


import sr.grpc.CurrencyNameG;
import sr.grpc.CurrencyRate;

import java.util.*;
import java.util.concurrent.*;
import java.util.stream.Collectors;

public class StockMarket {
    private static StockMarket ourInstance = new StockMarket();

    public static StockMarket getInstance() {
        return ourInstance;
    }

    private final HashMap<CurrencyNameG, Double> rates = new HashMap<>();
    private final LinkedList<BlockingQueue<CurrencyRate>> queues = new LinkedList<>();
    private final Random random = new Random();

    private StockMarket() {
        this.rates.put(CurrencyNameG.PLN, 1.0);
        this.rates.put(CurrencyNameG.USD, 0.28);
        this.rates.put(CurrencyNameG.GBP, 0.21);
        this.rates.put(CurrencyNameG.EUR, 0.23);
        this.rates.put(CurrencyNameG.JPY, 30.68);

        this.rates.keySet().parallelStream().forEach(name -> new Thread(() -> updateRate(name)).start());
    }

    private CurrencyRate buildRate(CurrencyNameG name, double rate){
        return CurrencyRate
                .newBuilder()
                .setName(name)
                .setRate(rate)
                .build();
    }


    public Set<CurrencyRate> getAllRates(){
        synchronized (rates){
            return rates.entrySet().parallelStream().map(e -> buildRate(e.getKey(), e.getValue())).collect(Collectors.toSet());
        }
    }

    public BlockingQueue<CurrencyRate> subscribe(){
        BlockingQueue<CurrencyRate> result = new LinkedBlockingQueue<>();
        synchronized (queues){
            queues.add(result);
        }
        return result;
    }

    private void updateRate(CurrencyNameG name){
        if(name == CurrencyNameG.PLN){
            return;
        }
        while(true){
            try {
                Thread.sleep(2500 + random.nextInt(5000));
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            double delta = random.nextDouble() / 10;
            int sign = random.nextBoolean() ? 1 : -1;
            double newValue;
            synchronized (rates) {
                double oldValue = rates.get(name);
                newValue = Math.max(0.01, oldValue + sign * oldValue * delta);
                rates.put(name, newValue);
            }
            CurrencyRate newRate = buildRate(name, newValue);
            synchronized (queues){
                queues.forEach(q -> {
                    try {
                        q.put(newRate);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                });
            }
        }
    }
}
