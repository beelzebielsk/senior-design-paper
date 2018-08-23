#lang pollen
◊(require "latex-commands.rkt")
◊(define p 3)
◊(define q 5)
◊(define n (* p q))
◊(define generator (add1 n))
◊(define modulus (* n n))
◊(define key-value 4)
◊(define public-key 
  (ensure-math 
    (format "(~a, ~a)"
            generator
            n)))
◊(define client (ensure-math "C"))
◊(define client-index "i")
◊(define current-server-index (ensure-math "h"))

◊title{What Does an RMS "Machine" Look Like?}

There is a set of inputs.

The inputs are fixed and unchanging throughout the duration of any
program that's run on an RMS machine.

An input ◊|input|, has a value, ◊${x}, and is created with a secret key,
◊|key|.

Let ◊key{j} be the jth bit of ◊|key|.

◊example{
    let ◊${◊key = 14 = (1100)_2}
    ◊eql{
        ◊key{0} = 0
        ◊key{1} = 0
        ◊key{2} = 1
        ◊key{3} = 1
    }
}

An input ◊|input|, is:

◊l{
- ◊${◊|input|.value = ◊encryption{x} = g^x r^n}, a paillier encryption of
◊${x}.
- ◊${◊|input|.bits = ◊encryption{◊key{i}x} for all bits i of ◊key}
}

◊note{For the rest of this paper, assume we are working with a
paillier cyrptosystem like the following:

◊eql{
    ◊|p| = 3, ◊|q| = 5
    ◊|n| = pq = ◊(* p q), n^2 = ◊|n|^2 = ◊|modulus|
    ◊key = ◊|key-value|, g = n+1 = ◊|generator|
}

◊example{
An input for ◊${x = 3, ◊key = ◊|key-value|}:

◊$${◊|input|.value = ◊encryption{x} = g^x r^n = ◊|generator|^3 r^◊|n|}
◊$${◊|input|.bits = 
    ◊apply[set]{◊(map (lambda (bit) 
        (encryption (key bit) "x"))
        (enumerate (number->bits key-value)))}
    }
}
}

The machine also has memory locations. The contents of memory
locations are generated during computation and can occasionally have
an initial value.

An RMS program is executed on two separate machines, ◊server{1},
◊server{2}. Each machine's memory looks just like the other machine's
memory, except that the contents are different. The contents of the
memory locations add up to some actual number.

◊note{Let ◊${x} be a number. Then ◊share{x} is an additive sharing of
◊${x}. An additive sharing of a number is two numbers, ◊${x_1, x_2} such
that ◊${x_1 + x_2 = x}. If ◊${x} is a value that's stored across the
memories of the machines, then each machine ◊${S_i} will have ◊${x_i} in
a memory location.

At times, I will also use ◊share{x} to refer to the particular share
that a machine has. So, for instance, when speaking about ◊server{1},
I will write ◊share{x} to mean ◊${x_1}, the share of x that ◊server{1}
has.}


A memory location ◊|location| of a value ◊${x} and secret key ◊key is:

◊small-note{
    ◊eql{
        ◊|location|.value = ◊share{x}
        ◊|location|.secret = ◊secret-share{x}
    }
    ◊note-content{Each of these is 1 number, not the pair of numbers
    associated with a share. It's the 1 number that the machine has,
    since memory locations reside on a machine.}
}

◊let[[(x 3)]]{
    ◊example{A memory location with ◊${x=◊x} and 
    ◊${◊key = ◊key-value}
    could be:

    Suppose ◊${◊share{◊|x|} = (-5, 8)} 
    Suppose ◊${◊secret-share{◊|x|} = ◊share{12} = (11, 1)}

    ◊table{
        ◊row{
            Machine 1
            Machine 2
        }
        ◊row{
            ◊${◊|location|.value = -5}
            ◊${◊|location|.value = 8}
        }
        ◊row{
            ◊${◊|location|.secret = 11}
            ◊${◊|location|.secret = 1}
        }
    }
    }
}

◊h1{RMS Operations}

The 1st operation is addition of two memory locations:

◊add-op{
    ◊in{◊share{x}, ◊secret-share{x}, ◊${◊|location|_1}}
    ◊in{◊share{y}, ◊secret-share{y}, ◊${◊|location|_2}}
    ◊out{◊share{x + y}, ◊secret-share{x + y}}
}


Each operation is performed on one machine only. So the memory
locations that we are adding are two different memory locations on one
machine. We're not adding the same memory location on different
machines or anything like that.

The output of each operation must produce a new memory location. So,
we must produce both a new value (◊share{x + y}) and a new secret
(◊secret-share{x + y}).

◊eql{
    ◊|location|_{out}.value = ◊|location|_1.value + ◊|location|_2.value
    ◊|location|_{out}.secret = ◊|location|_1.secret + ◊|location|_2.secret
}

◊example{Suppose:

◊let[[(x 3) (y 6)]]{
    ◊l{
    - We have ◊${x = ◊x, y = ◊y}
    - ◊${◊share{x} = (2, 1), ◊secret-share{x} = (7, 5)}
    - We want to add memory locations containing ◊share{x}, ◊share{y}
    to get a new memory location ◊share{x + y}.
    }}

    ◊table{
        ◊row{
            ◊server{1}
            ◊server{2}
            When Joined
        }
        ◊row{
            ◊${◊|location|_1 = ◊set{2, 7}}
            ◊${◊|location|_1 = ◊set{1, 5}}
            ◊set{3, 12}
        }
        ◊row{
            ◊${◊|location|_2 = ◊set{4, 16}}
            ◊${◊|location|_2 = ◊set{2, 8}}
            ◊set{6, 24}
        }
        ◊row{
            ◊${◊|location|_{out} = ◊set{6, 23}}
            ◊${◊|location|_{out} = ◊set{3, 13}}
            ◊set{9, 36}
        }
    }

    ◊${◊|location|_{out} = ◊set{◊share{x + y}, ◊secret-share{x + y}}} 
}

The second operation is multiplication by an input and a memory
location:

◊mult-op{
    ◊in{◊input}
    ◊in{
        ◊location,
        ◊share{y},
        ◊secret-share{y}
    }
    ◊out{
        ◊${◊|location|_out},
        ◊share{xy},
        ◊secret-share{xy}
    }
}

First, let us see how to combine ◊encryption{x}, ◊share{y} and
◊secret-share{y} to produce ◊share{xy}.

This process conists of creating ◊mult-share{xy}, and
◊mult-secret-share{xy}, and then transforming those multiplicative
shares into additive shares.

◊todo{table displaying creation of multiplicative shares, page 6}
◊congruent[#:modulus "n^2"]{
    ◊encryption{x}^{◊key y}
    g^{xy}
} because we have specially chosen ◊key to have this effect:
◊$${r^◊key = 1, g^{◊key x} = g^x}

We now have multilicative shares of ◊${xy}, which we denote by
◊mult-share{xy}.

To get ◊share{xy} from ◊mult-share{xy}, we use ◊|ddlog|.

◊todo{table showing use of DDLOG, page 6}

Let us denote this operation ◊|mult|. Now we can describe the
multiplication of an input, ◊input, and a memory location ◊location in
terms of ◊|mult|.

◊note{The result of ◊${\times(◊input, ◊location)} is a memory location
as well, so we must produce both ◊share{xy} and ◊secret-share{xy}.}

◊todo{Page 8 table}

◊h1{PIR: Private Information Retreival}

We have 3 machines here: a client, ◊client and 2 servers ◊server{1},
◊server{2}.

The servers have the same database: a string of bits, ◊${x}:

◊small-note{
    ◊${x = (x_0, x_1, ..., x_k)}
    ◊note-content{Think of this as an array of bits.}
}

The client wants to send out an index, ◊client-index and get back a
bit, ◊${x_i}, without any of the servers learning ◊|client-index|.

The client will:
◊ol{
- Create a set of paillier keys. The public key, ◊public-key, and the
secret key, ◊|key|.
- The client will send out:
    ◊l{
    - ◊encryption{◊bit{◊client-index j}} for all bits of ◊${i}.
    - ◊share{1}, ◊secret-share{} to the servers. This will allow the
    severs to perform some needed calculations, which will be
    explained later.
    ◊note{
    Sending out the bits of ◊client-index is somewhat expensive. The
    bits of ◊client-index are transported as inputs, so the actual
    number of things transmitted is on the order of 
    ◊${ bits of lambda X bits of i}.
    Specifically, what's sent out is 
    ◊set{
        ◊encryption{◊key{m} ◊bit{◊client-index j}} 
        for bits ◊${ m} of ◊|key|
    }
    }
}
}

The servers will:

◊ol{
- Receive client info.
- Run an RMS program, once for each possible index in the database
◊set{0, ..., k} with the ◊input{◊bit{◊client-index j} ◊key}, or 
◊input{1 - ◊bit{◊client-index j}, ◊key} as input. More explained later. Let
◊current-server-index
be the current index that the database is inspecting. The inputs to
the RMS program are the inputs representing the bits of ◊client-index and the
negation of those bits ◊${ 1 - ◊client-index}, and the bits of The output
of this program is ◊${ (◊share{1}, ◊secret-share{1}} whenever 
◊${ ◊client-index = ◊current-server-index}, and should output ◊${
◊share{0}, ◊secret-share{0}} otherwise. Let each of these outputs be
called ◊big-pi{i}.
- The server will then calculate the following sum:
◊sum{◊big-pi{j} ◊bit{x  j}}

This results in a memory location containing:

◊${ 
◊share{0 
    ◊bit{x 0} + 0 ◊bit{x 1} + ... + 
    1 ◊bit{x i} + 
    0 ◊bit{x i+1} + ... + 0 ◊bit{x l}}
◊secret-share{0 
    ◊bit{x 0} + 0 ◊bit{x 1} + ... + 
    1 ◊bit{x i} + 
    0 ◊bit{x i+1} + ... + 0 ◊bit{x l}}
=
◊share{◊bit{x i}}, ◊secret-share{◊bit{x i}}}

}

◊todo{manipulating inputs page 11}

◊h2{The bit string identity program}

This is the RMS program that takes in a bunch of inputs,
◊sub{◊input j}, where:

◊defn{
    ◊from{i}
    ◊to{A binary number. An index into a database.}
}
◊defn{
    ◊from{◊sub{◊input j}}
    ◊to{The input corresponding to ◊bit{i j}, the jth bit of ◊${ i}}.
}
