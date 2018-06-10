package streaming;

import akka.actor.AbstractActor;
import akka.actor.Props;
import akka.event.Logging;
import akka.event.LoggingAdapter;
import database.DatabaseWorker;
import messages.Response;
import messages.WorkRequest;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.UUID;

public class StreamManager extends AbstractActor {
    private final LoggingAdapter log = Logging.getLogger(getContext().getSystem(), this);

    @Override
    public Receive createReceive() {
        return receiveBuilder()
                .match(WorkRequest.class, r -> {
                    File f = new File("Books");
                    File[] matchingFiles = f.listFiles((dir, name) -> name.equals(new String(r.payload)));
                    if(matchingFiles == null || matchingFiles.length == 0){
                        getSender().tell(new Response(false, "".getBytes()), getSelf());
                    } else {
                        context().actorOf(Props.create(DatabaseWorker.class), UUID.randomUUID().toString());
                    }
                })
                .matchAny(o -> log.info(getClass()+ " received unknown message " + o))
                .build();
    }
}
