import org.jgroups.JChannel;
import org.jgroups.Message;
import org.jgroups.protocols.*;
import org.jgroups.protocols.pbcast.*;
import org.jgroups.stack.ProtocolStack;

import java.net.InetAddress;
import java.util.Map;

public class SimpleChannel {
    private JChannel channel;

    public SimpleChannel() throws Exception {
        channel = new JChannel(false);
        ProtocolStack stack = new ProtocolStack();
        channel.setProtocolStack(stack);
        stack.addProtocol(new UDP().setValue("mcast_group_addr", InetAddress.getByName("230.0.0.1")))
                .addProtocol(new PING())
                .addProtocol(new MERGE3())
                .addProtocol(new FD_SOCK())
                .addProtocol(new FD_ALL()
                        .setValue("timeout", 12000)
                        .setValue("interval", 3000))
                .addProtocol(new VERIFY_SUSPECT())
                .addProtocol(new BARRIER())
                .addProtocol(new NAKACK2())
                .addProtocol(new UNICAST3())
                .addProtocol(new STABLE())
                .addProtocol(new GMS())
                .addProtocol(new UFC())
                .addProtocol(new MFC())
                .addProtocol(new FRAG2())
                .addProtocol(new STATE())
                .addProtocol(new SEQUENCER())
                .addProtocol(new FLUSH());

        stack.init();
    }

    public void initialize(String cluster, Map<String, String> map) throws Exception {
        channel.setReceiver(new SimpleReceiver(channel, map));
        channel.connect(cluster);
        channel.getState(null, 0);
    }

    public void send(String s) throws Exception {
        Message msg = new Message(null, null, s);
        channel.send(msg);
    }
}
