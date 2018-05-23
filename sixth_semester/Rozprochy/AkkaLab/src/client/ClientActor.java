package client;

import akka.actor.AbstractActor;
import akka.actor.Props;
import akka.event.Logging;
import akka.event.LoggingAdapter;
import messages.MessageType;
import messages.Request;
import messages.Response;

public class ClientActor extends AbstractActor {
    private static final String SERVER_PATH = "akka.tcp://library@127.0.0.1:3552/user/server";
    private final LoggingAdapter log = Logging.getLogger(getContext().getSystem(), this);

    @Override
    public AbstractActor.Receive createReceive() {
        return receiveBuilder()
                .match(String.class, s -> {
                    String[] instr = s.split(" ", 2);
                    if(instr.length >= 2){
                        if(instr[0].equals("search")){
                            getContext().actorSelection(SERVER_PATH).tell(new Request(MessageType.SEARCH, instr[1].getBytes()), self());
                        }
                        if(instr[0].equals("order")){
                            getContext().actorSelection(SERVER_PATH).tell(new Request(MessageType.ORDER, instr[1].getBytes()), self());
                        }
                        if(instr[0].equals("stream")){
                            getContext().actorSelection(SERVER_PATH).tell(new Request(MessageType.STREAM, instr[1].getBytes()), self());
                        }
                    } else {
                        System.out.println("Provide space separated command and argument");
                    }
                })
                .match(Response.class, r -> {
                    if(r.success){
                        System.out.println(new String(r.payload));
                    } else {
                        System.out.println("Request failed");
                    }
                })
                .matchAny(o -> log.info(getClass()+ " received unknown message " + o))
                .build();
    }
}
