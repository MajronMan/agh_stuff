package Bank;

import sr.grpc.CurrencyNameG;
import sr.thrift.CurrencyNameT;

public class CurrencyAdapter {

    // thrift to grpc
    public static CurrencyNameG adapt(CurrencyNameT from){
        return CurrencyNameG.valueOf(from.name());
    }

    // grpc to thrift
    public static CurrencyNameT adapt(CurrencyNameG from){
        return CurrencyNameT.valueOf(from.name());
    }
}
