package orders;

import akka.actor.AbstractActor;
import akka.actor.ActorRef;
import akka.actor.Props;
import akka.event.Logging;
import akka.event.LoggingAdapter;
import messages.Response;
import messages.WorkRequest;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;

public class OrderManager extends AbstractActor {
    private final LoggingAdapter log = Logging.getLogger(getContext().getSystem(), this);

    @Override
    public Receive createReceive() {
        return receiveBuilder()
                .match(WorkRequest.class, r -> {
                    byte[] a = r.payload;
                    byte[] b = "\n".getBytes();
                    byte[] c = new byte[a.length + b.length];
                    System.arraycopy(a, 0, c, 0, a.length);
                    System.arraycopy(b, 0, c, a.length, b.length);
                    Files.write(Paths.get("orders.txt"), c, StandardOpenOption.APPEND);
                    getSender().tell(new Response(true, "Order successful".getBytes()), getSelf());
                })
                .matchAny(o -> log.info(getClass()+ " received unknown message " + o))
                .build();
    }
}
