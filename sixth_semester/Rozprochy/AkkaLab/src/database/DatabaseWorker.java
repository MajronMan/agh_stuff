package database;

import akka.actor.AbstractActor;
import akka.event.Logging;
import akka.event.LoggingAdapter;
import messages.Response;
import messages.database.DatabaseSupervisorRequest;
import messages.database.DatabaseWorkerRequest;

import java.io.*;

public class DatabaseWorker extends AbstractActor {
    private final LoggingAdapter log = Logging.getLogger(getContext().getSystem(), this);

    @Override
    public Receive createReceive() {
        return receiveBuilder()
                .match(DatabaseWorkerRequest.class, r -> {
                    FileReader fileReader = new FileReader(r.databaseName);
                    BufferedReader bufferedReader = new BufferedReader(fileReader);
                    String line;
                    boolean failed = true;

                    while ((line = bufferedReader.readLine()) != null) {
                        int sep = line.lastIndexOf(" ");
                        String price = line.substring(sep + 1);
                        String name = line.substring(0, sep);
                        System.out.println(name);
                        if (r.phrase.equals(name)) {
                            failed = false;
                            getSender().tell(new Response(true, price.getBytes()), getSelf());
                        }
                    }
                    if(failed) {
                        getSender().tell(new Response(false, "".getBytes()), getSelf());
                    }
                    bufferedReader.close();
                })
                .matchAny(o -> log.info(getClass() + " received unknown message " + o))
                .build();
    }
}
