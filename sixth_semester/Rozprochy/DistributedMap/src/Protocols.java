import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

public class Protocols {
    public static String put(String key, String value) {
        return "put " + key + " " + value;
    }

    public static String put(Map.Entry<String, String> e){
        return put(e.getKey(), e.getValue());
    }

    public static String get(String key) {
        return "get " + key;
    }

    public static String remove(String key) {
        return "remove " + key;
    }

    public static String containsKey(String key) {
        return "containsKey " + key;
    }

    public static String getState() {
        return "getState";
    }

    public static List<String> mapToCommands(Map<String, String> map) {
        return map.entrySet().stream().map(Protocols::put).collect(Collectors.toList());
    }

    public static Function<Map<String, String>, Object> parse(String msg) {
        String[] split = msg.split(" ");
        String cmd = split[0];

        if (cmd.equals("put")) {
            if(split.length < 3) {
                return null;
            }
            final String key = split[1], value = split[2];
            return map -> map.put(key, value);
        }
        if (cmd.equals("get")) {
            if(split.length < 2) {
                return null;
            }
            final String key = split[1];
            return map -> map.get(key);
        }
        if (cmd.equals("remove")) {
            if(split.length < 2) {
                return null;
            }
            final String key = split[1];
            return map -> map.remove(key);
        }
        if(cmd.equals("containsKey")) {
            if(split.length < 2) {
                return null;
            }
            final String key = split[1];
            return map -> map.containsKey(key);
        }
        if(cmd.equals("entrySet")) {
            return Map::entrySet;
        }
        return null;
    }
}
