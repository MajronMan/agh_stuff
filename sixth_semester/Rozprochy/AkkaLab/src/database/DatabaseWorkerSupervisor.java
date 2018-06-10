package database;

import akka.actor.*;

import static akka.actor.SupervisorStrategy.escalate;
import static akka.actor.SupervisorStrategy.restart;
import akka.event.Logging;
import akka.event.LoggingAdapter;
import akka.japi.pf.DeciderBuilder;
import messages.Response;
import messages.database.DatabaseSupervisorRequest;
import messages.database.DatabaseWorkerRequest;
import scala.concurrent.duration.Duration;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.UUID;

public class DatabaseWorkerSupervisor extends AbstractActor {
    private ActorRef workerA;
    private ActorRef workerB;
    private ActorRef customer;
    private boolean firstMessage = true;
    private final LoggingAdapter log = Logging.getLogger(getContext().getSystem(), this);

    @Override
    public Receive createReceive() {
        return receiveBuilder()
                .match(DatabaseSupervisorRequest.class, r -> {
                    customer = getSender();
                    workerA.tell(new DatabaseWorkerRequest("db1.txt", r.phrase), getSelf());
                    workerB.tell(new DatabaseWorkerRequest("db2.txt", r.phrase), getSelf());
                })
                .match(Response.class, r -> {
                    if (firstMessage) {
                        firstMessage = false;
                        if (r.success) {
                            finish(r);
                        } else {
                            context().stop(getSender());
                        }
                    } else {
                        finish(r);
                    }

                })
                .matchAny(o -> log.info(getClass() + " received unknown message " + o))
                .build();
    }

    private void finish(Response response) {
        context().stop(workerA);
        context().stop(workerB);
        customer.tell(response, getSelf());
        context().stop(getSelf());
    }

    @Override
    public void preStart() throws Exception {
        workerA = context().actorOf(Props.create(DatabaseWorker.class), UUID.randomUUID().toString());
        workerB = context().actorOf(Props.create(DatabaseWorker.class), UUID.randomUUID().toString());
    }

    private static SupervisorStrategy strategy
            = new OneForOneStrategy(10, Duration.create("1 minute"), DeciderBuilder
            .match(FileNotFoundException.class, e -> escalate())
            .match(IOException.class, e -> restart())
            .matchAny(o -> restart())
            .build());

    @Override
    public SupervisorStrategy supervisorStrategy() {
        return strategy;
    }
}
