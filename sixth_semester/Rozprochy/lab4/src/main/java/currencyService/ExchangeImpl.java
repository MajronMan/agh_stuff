package currencyService;

import io.grpc.stub.StreamObserver;
import sr.grpc.*;

import java.util.List;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.stream.Collectors;

public class ExchangeImpl extends ExchangeGrpc.ExchangeImplBase {
    @Override
    public void currentRates(RateStateRequest request, io.grpc.stub.StreamObserver<RateStateResponse> responseObserver){
        RateStateResponse.Builder responseBuilder = RateStateResponse.newBuilder();
        Set<CurrencyRate> requestedRates = StockMarket.getInstance()
                .getAllRates()
                .parallelStream()
                .filter(r -> request.getCurrenciesList().contains(r.getName()))
                .collect(Collectors.toSet());
        responseBuilder.addAllRates(requestedRates);
        responseObserver.onNext(responseBuilder.build());
        responseObserver.onCompleted();
    }

    @Override
    public void streamRates(RateStateRequest request, StreamObserver<CurrencyRate> responseObserver){
        BlockingQueue<CurrencyRate> queue = StockMarket.getInstance().subscribe();
        List<CurrencyNameG> requestedNames = request.getCurrenciesList();
        while (true){
            try {
                CurrencyRate newRate = queue.take();
                if(requestedNames.contains(newRate.getName())){
                    responseObserver.onNext(newRate);
                }
            } catch (InterruptedException e) {
                e.printStackTrace();
                break;
            }
        }
        responseObserver.onCompleted();
    }
}
