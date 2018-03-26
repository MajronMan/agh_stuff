import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class SimpleStringMapImpl implements SimpleStringMap {
    private HashMap<String, String> innerCopy = new HashMap<>();
    private SimpleChannel channel;

    public SimpleStringMapImpl(String cluster) throws Exception {
        channel = new SimpleChannel();
        channel.initialize(cluster, innerCopy);
    }

    @Override
    public boolean containsKey(Object key) {
        return innerCopy.containsKey(key);
    }

    @Override
    public String get(String key) {
        return innerCopy.get(key);
    }

    @Override
    public String get(Object key) {
        if(!(key instanceof String)){
            return null;
        }
        return get((String) key);
    }

    @Override
    public int size() {
        return innerCopy.size();
    }

    @Override
    public boolean isEmpty() {
        return innerCopy.isEmpty();
    }

    @Override
    public boolean containsValue(Object value) {
        return innerCopy.containsValue(value);
    }

    @Override
    public String put(String key, String value) {
        try {
            send(Protocols.put(key, value));
            return value;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public void putAll(Map<? extends String, ? extends String> m) {
        innerCopy.putAll(m);
    }

    @Override
    public String remove(String key) {
        String result = innerCopy.get(key);
        try {
            send(Protocols.remove(key));
            return result;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    @Override
    public String remove(Object key) {
        if(!(key instanceof String)){
            return null;
        }
        return remove((String) key);
    }

    public Set<Entry<String, String>> entrySet(){
        return innerCopy.entrySet();
    }

    @Override
    public void clear(){
        innerCopy.clear();
    }

    @Override
    public Map<String, String> getInnerCopy() {
        return innerCopy;
    }

    @Override
    public Set<String> keySet() {
        return innerCopy.keySet();
    }

    @Override
    public Collection<String> values() {
        return innerCopy.values();
    }

    private void send(String msg) throws Exception {
        channel.send(msg);
    }
}
