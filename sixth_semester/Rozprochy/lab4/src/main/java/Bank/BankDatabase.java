package Bank;

import sr.grpc.CurrencyNameG;
import sr.thrift.BankClient;
import sr.thrift.GUID;
import sr.thrift.InvalidGuid;
import sr.thrift.CurrencyNameT;

import java.util.*;

public class BankDatabase {
    private final CurrencyClient currencyClient;
    private final Map<GUID, BankClient> clientMap = new HashMap<>();

    public BankDatabase(int exchangePort, Set<CurrencyNameG> currencyNames){
        currencyClient = new CurrencyClient("localhost", exchangePort, currencyNames);
    }

    public void register(BankClient client) throws InvalidGuid{
        if(clientPresent(client.guid)){
            throw new InvalidGuid(client.guid, "Client already present!");
        } else {
            synchronized (clientMap){
                clientMap.put(client.guid, client);
            }
        }
    }

    public boolean clientPresent(GUID guid) {
        return clientMap.containsKey(guid);
    }

    public BankClient getBankClient(GUID guid) throws InvalidGuid{
        System.out.println("getBankClient " + guid);
        BankClient client = clientMap.get(guid);
        if(client == null){
            throw new InvalidGuid(guid, "No such client!");
        }
        return client;
    }

    public double getRate(CurrencyNameT name){
        return currencyClient.getRates().get(CurrencyAdapter.adapt(name));
    }
}
