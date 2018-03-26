import java.util.Map;
import java.util.Set;

public interface SimpleStringMap extends Map<String, String> {
    boolean containsKey(Object key);

    String get(String key);

    String put(String key, String value);

    String remove(String key);

    Set<Entry<String, String>> entrySet();

    void clear();

    Map<String, String> getInnerCopy();
}
