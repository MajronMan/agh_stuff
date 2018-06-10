package server;

import akka.actor.*;
import akka.event.Logging;
import akka.event.LoggingAdapter;
import akka.japi.pf.DeciderBuilder;
import database.DatabaseManager;
import messages.Request;
import messages.WorkRequest;
import orders.OrderManager;
import scala.concurrent.duration.Duration;
import static akka.actor.SupervisorStrategy.restart;

public class ServerActor extends AbstractActor {

    // for logging
    private final LoggingAdapter log = Logging.getLogger(getContext().getSystem(), this);
    private ActorRef databaseManager;
    private ActorRef orderManager;

    // must be implemented -> creates initial behaviour
    @Override
    public AbstractActor.Receive createReceive() {
        return receiveBuilder()
                .match(String.class, System.out::println)
                .match(Request.class, r -> handleRequest(r, getSender()))
                .matchAny(o -> log.info(getClass()+ " received unknown message " + o))
                .build();
    }

    // optional
    @Override
    public void preStart() throws Exception {
        databaseManager = context().actorOf(Props.create(DatabaseManager.class), "databaseManager");
        orderManager = context().actorOf(Props.create(OrderManager.class), "orderManager");
    }

    private static SupervisorStrategy strategy
            = new OneForOneStrategy(10, Duration.create("1 minute"), DeciderBuilder.
                    matchAny(o -> restart()).
                    build());

    @Override
    public SupervisorStrategy supervisorStrategy() {
        return strategy;
    }

    private void handleRequest(Request r, ActorRef sender) {
        switch (r.type) {
            case SEARCH:
                databaseManager.tell(new WorkRequest(r.payload), sender);
                break;
            case ORDER:
                orderManager.tell(new WorkRequest(r.payload), sender);
                break;
            case STREAM:
//                handleStream(r.payload, getSender());
                break;
        }
    }

}
