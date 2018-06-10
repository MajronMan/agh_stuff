package database;

import akka.actor.*;
import akka.event.Logging;
import akka.event.LoggingAdapter;
import akka.japi.pf.DeciderBuilder;
import messages.database.DatabaseSupervisorRequest;
import messages.WorkRequest;
import scala.concurrent.duration.Duration;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.UUID;

import static akka.actor.SupervisorStrategy.escalate;
import static akka.actor.SupervisorStrategy.stop;

public class DatabaseManager extends AbstractActor {
    private final LoggingAdapter log = Logging.getLogger(getContext().getSystem(), this);

    @Override
    public Receive createReceive() {
        return receiveBuilder()
                .match(WorkRequest.class, r -> {
                    ActorRef supervisor = context().actorOf(Props.create(DatabaseWorkerSupervisor.class), UUID.randomUUID().toString());
                    supervisor.tell(new DatabaseSupervisorRequest(new String(r.payload)), getSender());
                })
                .matchAny(o -> log.info(getClass()+ " received unknown message " + o))
                .build();
    }

    private static SupervisorStrategy strategy
            = new OneForOneStrategy(10, Duration.create("1 minute"), DeciderBuilder
            .matchAny(o -> stop())
            .build());

    @Override
    public SupervisorStrategy supervisorStrategy() {
        return strategy;
    }
}
