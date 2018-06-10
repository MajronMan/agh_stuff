const thrift = require('thrift');
const ttypes = require('../gen-nodejs/bank_types');
const AccountCreator = require('../gen-nodejs/AccountCreator');
const PremiumManager = require('../gen-nodejs/PremiumManager');
const StandardManager = require('../gen-nodejs/StandardManager');
const assert = require('assert');
const readline = require('readline')

const getKeyByValue = (object, value) => 
    Object.keys(object).find(key => object[key] === value);


const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });


const repl = clients => {
    rl.question(`Select an option:
1: register
2: check account balance
3: loan cost [premium only]\n`, 
        ans => {
        const op = ans.trim();
        if(op === '1'){
            register(clients)
        } else if(op === '2'){
            checkAccountBalance(clients)
        } else if(op === '3'){
            checkLoanCost(clients)
        } else {
            console.log("Invalid operation")
            repl(clients)
        }
    });
}

const register = clients => {
    rl.question("Enter space separated name, surname, pesel, salary and inital balance\n", 
    ans => {
        res = ans.split(" ");
        clients.creator.registerClient( 
            new ttypes.PersonalData({
                name: res[0], 
                surname: res[1],
                pesel: {value: parseInt(res[2])},
                salary: {cents: parseInt(res[3]) * 100} 
            }), 
            new ttypes.Money({cents: parseInt(res[4])*100}),
            (err, response) => {
                if(err){
                    console.log(`${err.message} ${err.reason}`)
                } else {
                console.log(`Your new account is ${getKeyByValue(ttypes.AccountType, response.type)} with GUID:\n${response.guid.value}`)
                }
                repl(clients)
            }
        )
    })
}

const checkAccountBalance = clients => {
    rl.question("Enter your GUID:\n", ans => {
        const guid = ans.trim();
        rl.question("Enter your account type [s for standard and p for premium]\n", ans => {
            const t = ans.trim();
            if(t === 's'){
                clients.standard.getBalance({value: guid}, (err, response) => {
                    if(err){
                        console.log(`${err.message} ${err.reason}`)
                    } else {
                        console.log(`${response.cents/100}`)
                    }
                    repl(clients)
                })
            } else if(t === 'p'){
                clients.premium.getBalance({value: guid}, (err, response) => {
                    if(err){
                        console.log(`${err.message} ${err.reason}`)
                    } else {
                        console.log(`${response.cents/100}`)
                    }
                    repl(clients)
                })
            } else {
                console.log("Invalid account type")
                repl(clients)
            }
        })
    })
}

const checkLoanCost = clients => {
    rl.question("Enter your GUID:\n", ans => {
        const guid = ans.trim();
        rl.question("Enter desired loan value and currency\n", ans => {
            const spl = ans.split(" ");
            const val = parseInt(spl[0])
            const currency = ttypes.CurrencyNameT[spl[1]] || ttypes.CurrencyNameT['PLN']
            const request = new ttypes.LoanRequest({
                amount: {cents: val * 100},
                currency: currency,
                guid: {value: guid}
            })
            clients.premium.askForLoan(request, (err, response) => {
                if(err){
                    console.log(`${err.message} ${err.reason}`)
                } else {
                    if(response.accepted){
                        console.log(`Loan cost is ${response.baseCost.cents/100} PLN and ${response.targetCost.cents/100} ${getKeyByValue(ttypes.CurrencyNameT, currency)}`)
                    } else {
                        console.log("Request was rejected")
                    }
                }
                repl(clients)
            })
        })
    })
}

const transport = thrift.TBufferedTransport;
const protocol = thrift.TBinaryProtocol;
const port = parseInt(process.argv[2])
console.log(`Opening connection on ${port}`)

const connection = thrift.createConnection("localhost", port, {
    transport : transport,
    protocol : protocol
});

connection.on('error', function(err) {
    assert(false, err);
});

const multiplexer = new thrift.Multiplexer();
const creator = multiplexer.createClient('creator', AccountCreator, connection);
const standard = multiplexer.createClient('standard', StandardManager, connection);
const premium = multiplexer.createClient('premium', PremiumManager, connection);

repl({creator, standard, premium})
