import org.jgroups.*;
import org.jgroups.util.Util;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class SimpleReceiver extends ReceiverAdapter {
    private final Map<String, String> map;
    private final JChannel channel;

    SimpleReceiver(JChannel channel, Map<String, String> map) {
        this.channel = channel;
        this.map = map;
    }

    @Override
    public void viewAccepted(View view) {
        System.out.println(view);
        if (!(view instanceof MergeView)) {
            return;
        }
        Thread t = new Thread(() -> {
            View tmp = ((MergeView) view).getSubgroups().get(0);
            Address local = channel.getAddress();
            if (tmp.getMembers().contains(local)) {
                System.out.println("Member of new primary partition, doing nothing");
                return;
            }
            try {
                System.out.println("Not a member of new primary partition, will re-acquire the state");
                channel.getState(null, 30000);
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
        t.start();
    }

    @Override
    public void receive(Message msg) {
        String cmd = (String) msg.getObject();
        System.out.println("Received message from " + msg.getSrc() + " - " + cmd);

        Objects.requireNonNull(Protocols.parse(cmd)).apply(map);
    }

    @Override
    public void getState(OutputStream output) throws Exception {
        synchronized (map){
            Util.objectToStream(Protocols.mapToCommands(map), new DataOutputStream(output));
        }
    }

    @Override
    public void setState(InputStream input) throws Exception {
        List<String> commands = (List<String>) Util.objectFromStream(new DataInputStream(input));
        synchronized (map){
            map.clear();
            commands.stream().map(Protocols::parse).filter(Objects::nonNull).forEach(f -> f.apply(map));
        }
    }
}
