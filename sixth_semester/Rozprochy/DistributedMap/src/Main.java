import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Map;
import java.util.function.Function;

public class Main {
    public static void main(String[] args) throws Exception {
        System.setProperty("java.net.preferIPv4Stack","true");
        SimpleStringMap map = new SimpleStringMapImpl("Klusker");
        InputStreamReader inp = new InputStreamReader(System.in);
        BufferedReader reader = new BufferedReader(inp);
        String msg;

        for(boolean c = true; c; c = !msg.equals("quit")){
            msg = reader.readLine();
            Function<Map<String, String>, Object> action = Protocols.parse(msg);
            if(action != null) {
                System.out.println(action.apply(map));
            } else {
                System.out.println("Wrong command");
            }
        }
        reader.close();
    }
}
