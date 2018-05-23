package streaming;

import akka.actor.AbstractActor;
import akka.event.Logging;
import akka.event.LoggingAdapter;
import messages.Response;
import messages.database.DatabaseWorkerRequest;

import java.io.BufferedReader;
import java.io.FileReader;

public class StreamWorker extends AbstractActor {
    private final LoggingAdapter log = Logging.getLogger(getContext().getSystem(), this);

    @Override
    public AbstractActor.Receive createReceive() {
        return receiveBuilder()
                .match(DatabaseWorkerRequest.class, r -> {
                    FileReader fileReader = new FileReader(r.databaseName);
                    BufferedReader bufferedReader = new BufferedReader(fileReader);
                    String line;

                    while ((line = bufferedReader.readLine()) != null) {
                        String[] data = line.split(" ");
                        if(r.phrase.equals(data[0])){
                            getSender().tell(new Response(true, data[1].getBytes()), getSelf());
                        }
                    }
                    getSender().tell(new Response(false, "".getBytes()), getSelf());
                    bufferedReader.close();
                })
                .matchAny(o -> log.info(getClass() + " received unknown message " + o))
                .build();
    }
}

