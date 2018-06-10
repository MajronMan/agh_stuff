package Bank;

import Bank.services.AccountCreatorService;
import Bank.services.PremiumManagerService;
import Bank.services.StandardManagerService;
import org.apache.thrift.TMultiplexedProcessor;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.server.TServer;
import org.apache.thrift.server.TSimpleServer;
import org.apache.thrift.transport.TServerSocket;
import org.apache.thrift.transport.TServerTransport;
import org.apache.thrift.transport.TTransportException;
import sr.grpc.CurrencyNameG;
import sr.thrift.AccountCreator;
import sr.thrift.PremiumManager;
import sr.thrift.StandardManager;

import java.util.*;

public class Bank {
    public static void main(String[] args) throws TTransportException {
        Scanner s = new Scanner(System.in);
        System.out.println("Enter bank port");
        int bankPort = s.nextInt();
        System.out.println("Enter currency exchange port");
        int exchangePort = s.nextInt();
        s.nextLine();
        System.out.println("Enter space separated currency names");
        String input = s.nextLine();
        String[] currencies = input.split(" ");
        Set<CurrencyNameG> currencyNames = new HashSet<>();
        for (String currency : currencies) {
            CurrencyNameG name;
            try{
                name = CurrencyNameG.valueOf(currency);
                currencyNames.add(name);
            } catch (IllegalArgumentException e){
                System.out.println("No currency for " + currency);
            }
        }
        currencyNames.add(CurrencyNameG.PLN);

        BankDatabase database = new BankDatabase(exchangePort, currencyNames);

        AccountCreator.Processor<AccountCreatorService> accountCreatorServiceProcessor =
                new AccountCreator.Processor<>(new AccountCreatorService(database));
        StandardManager.Processor<StandardManagerService> standardAccountManagerServiceProcessor = new StandardManager.Processor<>(new StandardManagerService(database));
        PremiumManager.Processor<PremiumManagerService> premiumAccountManagerServiceProcessor =
                new PremiumManager.Processor<>(new PremiumManagerService(database, currencyNames));

        TServerTransport serverTransport = new TServerSocket(bankPort);
        TProtocolFactory protocolFactory = new TBinaryProtocol.Factory();

        TMultiplexedProcessor multiplex = new TMultiplexedProcessor();

        multiplex.registerProcessor("creator", accountCreatorServiceProcessor);
        multiplex.registerProcessor("standard", standardAccountManagerServiceProcessor);
        multiplex.registerProcessor("premium", premiumAccountManagerServiceProcessor);

        TServer server = new TSimpleServer(new TServer.Args(serverTransport)
                .protocolFactory(protocolFactory)
                .processor(multiplex)
        );
        System.out.println("Starting Bank");
        server.serve();
    }
}
