class Fork {
    constructor(){
        this.state = 0
    }

    acquire(cb) {
    // zaimplementuj funkcje acquire, tak by korzystala z algorytmu BEB
    // (http://pl.wikipedia.org/wiki/Binary_Exponential_Backoff), tzn:
    // 1. przed pierwsza proba podniesienia widelca Filozof odczekuje 1ms
    // 2. gdy proba jest nieudana, zwieksza czas oczekiwania dwukrotnie
    //    i ponawia probe itd.
        
        while(this.state === 1){
            
        }
        this.state = 1
    }

    release(cb) {
        this.state = 0
    }
}

class Philosopher {
    constructor(id, forks){
        this.id = id
        this.forks = forks
        this.left = id % forks.length 
        this.right = (id + 1) % forks.length
    }

    startNaive(count) {

    }

    startAsym(count) {
    // zaimplementuj rozwiazanie asymetryczne
    // kazdy filozof powinien 'count' razy wykonywac cykl
    // podnoszenia widelcow -- jedzenia -- zwalniania widelcow
    }

    startConductor(count) {
    // zaimplementuj rozwiazanie z kelnerem
    // kazdy filozof powinien 'count' razy wykonywac cykl
    // podnoszenia widelcow -- jedzenia -- zwalniania widelcow
    }
}


const N = 5
const forks = Array(N).fill(new Fork())
const  philosophers = Array(N).fill(1).map((x, y) => x + y - 1).map(x => new Philosopher(x, forks))

philosophers.forEach(x => x.startNaive(10))
