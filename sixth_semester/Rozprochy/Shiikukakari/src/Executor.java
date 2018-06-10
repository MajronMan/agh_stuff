/**
 * A simple example program to use DataMonitor to start and
 * stop executables based on a znode. The program watches the
 * specified znode and saves the data that corresponds to the
 * znode in the filesystem. It also starts the specified program
 * with the specified arguments when the znode exists and kills
 * the program if the znode goes away.
 */

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.Scanner;
import java.util.stream.Collectors;

import org.apache.log4j.PropertyConfigurator;
import org.apache.zookeeper.KeeperException;
import org.apache.zookeeper.WatchedEvent;
import org.apache.zookeeper.Watcher;
import org.apache.zookeeper.ZooKeeper;

public class Executor
        implements Watcher, Runnable, DataMonitor.DataMonitorListener {
    String znode;

    DataMonitor dm;

    ZooKeeper zk;

    String filename;

    String exec[];

    Process child;

    public Executor(String hostPort, String znode, String filename,
                    String exec[]) throws KeeperException, IOException {

        this.filename = filename;
        this.exec = exec;
        this.znode = znode;
        zk = new ZooKeeper(hostPort, 1000, this);
        dm = new DataMonitor(zk, znode, null, this);
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        if (args.length < 4) {
            System.err
                    .println("USAGE: Executor hostPort znode filename program [args ...]");
            System.exit(2);
        }
        String hostPort = args[0];
        String znode = args[1];
        String filename = args[2];
        String exec[] = new String[args.length - 3];
        System.arraycopy(args, 3, exec, 0, exec.length);
        Properties props = new Properties();
        try {
            InputStream configStream = Executor.class.getClassLoader().getResourceAsStream("log4j.properties");
            props.load(configStream);
            configStream.close();
        } catch (IOException e) {
            System.out.println("Error: Cannot laod configuration file ");
        }
        props.setProperty("log4j.appender.FILE.file", "zoo.log");
        PropertyConfigurator.configure(props);

        try {
            new Executor(hostPort, znode, filename, exec).run();
        } catch (KeeperException | IOException e) {
            e.printStackTrace();
        }
    }

    /***************************************************************************
     * We do process any events ourselves, we just need to forward them on.
     *
     *
     */
    public void process(WatchedEvent event) {
        System.out.println(event);
        dm.process(event);
    }

    public void run() {
        Scanner s = new Scanner(System.in);
        synchronized (this) {
            while (!dm.dead) {
                String l = s.nextLine().trim();
                if(l.equals("ls")){
                    traverse(znode).print();
                }
                if(l.equals("exit") || l.equals("quit")){
                    closing(0);
                    if(child != null){
                        child.destroy();
                    }
                    break;
                }
            }
        }
    }

    public void closing(int rc) {
        synchronized (this) {
            notifyAll();
        }
    }

    @Override
    public void getChildren(List<String> children) {
        System.out.println("Node " + znode + " has " + children.size() + " children");
    }

    private class Node {
        final int depth;
        final String path;
        final List<Node> children;

        public Node(int depth, String path) {
            this.depth = depth;
            this.path = path;
            children = new ArrayList<>();
        }

        public Node getChildren(ZooKeeper zk) {
            try {
                children.addAll(
                        zk.getChildren(path, false)
                                .stream()
                                .map(name -> new Node(depth + 1, path + "/" + name))
                                .collect(Collectors.toList())
                );
                children.forEach(c -> c.getChildren(zk));
                return this;
            } catch (InterruptedException | KeeperException e) {
                e.printStackTrace();
            }
            return this;
        }

        public int countChildren() {
            return children.stream().map(Node::countChildren).reduce(children.size(), Integer::sum);
        }

        public void print() {
            String tab = new String(new char[depth]).replace('\0', '\t');
            System.out.println(tab + path);
            children.forEach(Node::print);
        }
    }

    private Node traverse(String node) {
        return new Node(0, node).getChildren(zk);
    }

    static class StreamWriter extends Thread {
        OutputStream os;

        InputStream is;

        StreamWriter(InputStream is, OutputStream os) {
            this.is = is;
            this.os = os;
            start();
        }

        public void run() {
            byte b[] = new byte[80];
            int rc;
            try {
                while ((rc = is.read(b)) > 0) {
                    os.write(b, 0, rc);
                }
            } catch (IOException e) {
            }

        }
    }


    public void exists(byte[] data) {
        if (data == null) {
            if (child != null) {
                System.out.println("Killing process");
                child.destroy();
                try {
                    child.waitFor();
                } catch (InterruptedException e) {
                }
            }
            child = null;
        } else {
            if (child != null) {
                System.out.println("Stopping child");
                child.destroy();
                try {
                    child.waitFor();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
            try {
                FileOutputStream fos = new FileOutputStream(filename);
                fos.write(data);
                fos.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
            try {
                System.out.println("Starting child");
                child = Runtime.getRuntime().exec(exec);
                new StreamWriter(child.getInputStream(), System.out);
                new StreamWriter(child.getErrorStream(), System.err);
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }
}