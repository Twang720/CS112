#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"$Id: hzip.st,v 1.0 2018-02-25 18:57:46-08 - - $"

"newline character"
nl := Character nl.


"==================================================================="
"==================================================================="
"=================SUBCLASSES AND CLASS EXTENSIONS==================="
"==================================================================="
"==================================================================="

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 16]
   ]
]

"to get bit arrays of each character"
Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   "assumes zero based, so increments 1"
   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]

]

Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

   "assuming its a leaf, print out 0 + ASCII using outbits"
   postorder: visitor [
   "once it reaches leaf, pass char and bit to visitor"
    visitor value: char value: 0. 
   ]

]

Leaf subclass: Tree [
   |left right|
   getLeft [ ^ left ]
   getRight [ ^ right ]

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

   postorder: visitor [
      left postorder: visitor.
      right postorder: visitor.
      "now you assume its an internal node, so to outbits print 1"
      visitor value: self char value: 1.
   ]

]

"==================================================================="
"==================================================================="
"===========================MAIN BLOCKS============================="
"==================================================================="
"==================================================================="

"arrays and other global variables"
freqtable := ZeroArray new: 257.
encodingtable := ZeroArray new: 257.
priority := SortedCollection new.
argu := false. 
argd := false. 
argc := false.
argt := false. 
EOF := 256. 

"for reading characters into frequency table"
readInFile := [:freqtable :infile | [infile atEnd not] whileTrue: [
   |ordChar|
   ordChar := infile next asInteger.
   freqtable incr: ordChar. ]
  ].

"for -c option"
compress := [ :outfile :infile |

  "intialized to nil"
  |encodingtree bitOutput|

  "adding nonzero counts to priority queue"
  freqtable keysAndValuesDo: [:key :value |
   (value notNil and: [value > 0]) ifTrue: [
   priority add: (Leaf new: key count: value)
  ]].

  "now, pop off priorty queue and build into a tree"
  [priority size > 1] whileTrue:[
    |left right|

    encodingtree isNil 
    ifTrue: [ 
      "get left and right child"
      left := priority removeFirst.
      right := priority removeFirst.
      "make new tree" 
      encodingtree := Tree new: (left char) 
      count: (left count) + (right count) left: left right: right.
      "insert new leaf onto priority queue"
      priority add: encodingtree. 
    ] ifFalse: [
      "get left and right child"
      |a b|
      a := priority removeFirst.
      b := priority removeFirst.
      a <= b ifTrue: [ left := a. right := b.] 
      ifFalse: [left := b. right := a. ].
      encodingtree := Tree new: (left char) count: (left count) 
      + (right count) left: left right: right. 
      "insert new leaf onto priority queue"
      priority add: encodingtree. 
    ].
  ].
  
   "in order traversal to build encoding string"
   encodingtree inorder: [:char :string |
   encodingtable at: char put: string. ] prefix: ''.


  "print encoding table, if -t is specified"
  argt ifTrue: [
  encodingtable keysAndValuesDo: [:key :value |
   (value notNil and: [value > 0]) ifTrue: [
   |temp char|
   char := key asCharacter.  
   "to make it right justified"
   temp := freqtable at: key.
   temp := (temp printPaddedWith: $ to:  6).  
   (char isPrint and: [((char asInteger) = 32) not]) 
   ifTrue:[
   stdout << ' ' << char << ' ' << temp << ' ' << value << nl. 
   ] ifFalse: [
   "special case for EOF character and space"
   char asInteger = EOF ifTrue: [
        char := 'EOF'.
        stdout << char << temp << ' ' << value << nl.
   ] ifFalse: [
        "replace 16r with x0, using subStrings and OrderedCollections"
        "if the hex value greater than 16, 
          don't add leading zero, otherwise do"
        ((char asInteger) = 32) ifTrue: [
        stdout << 'x20' << temp << ' ' << value << nl.
        ] ifFalse: [
        char visible size > 4 ifTrue: [
          char := 'x', (char visible subStrings: '16r') first.
        ] ifFalse: [
          char := 'x0', (char visible subStrings: '16r') first.
        ].    
        stdout << char << temp << ' ' << value << nl. ] ].]]].].


  "for writing bit encodingtree to file"
  bitOutput := OutBits new: outfile. 

  argc ifTrue: [
  "write encoding tree to output file"
  encodingtree postorder: [:char :outbit|
    |count|
    count := 8. 
    "outbit = 0 => leaf node, so write out 
    in big endian"
    outbit = 0 ifTrue: [
    "write bit 0 to denote leaf node"
    bitOutput writeBit: 0. 
    "loop that writes from MSB to LSB of char"
    [count > 0] whileTrue: [
      bitOutput writeBit: (char asInteger bitAt: count). 
      count := count - 1. ].
      "adds byte to stream"
      "extra bit for EOF and 0"
      char = 0 ifTrue: [
       bitOutput writeBit: 0.
       ].
      char = EOF ifTrue: [
       bitOutput writeBit: 1.
       ]. 
    ] ifFalse: [
      "write out 1 to denote interior node"
      bitOutput writeBit: 1.
    ]. ]. 

   "1 to indicate file is done writing out"
   bitOutput writeBit: 1.

   "reset file pointer"
   infile reset.


   "write out encoded version of each character 
    as present in infile to outfile"
   [infile atEnd not] whileTrue: [
   |ordChar strCt str|
   ordChar := infile next asInteger.
   str := encodingtable at: ordChar. 
   strCt := 1.   
   [strCt <= str size] whileTrue: [
      bitOutput writeBit: ((str at: strCt) asString asInteger). 
      strCt := strCt + 1. 
      ].].
    
   infile atEnd ifTrue: [
   |ordChar strCt str|
   str := encodingtable at: EOF. 
   strCt := 1.   
   [strCt <= str size] whileTrue: [
      bitOutput writeBit: ((str at: strCt) asString asInteger).
      strCt := strCt + 1. ]. ].
      "padding with zero bits"
      bitOutput flushByte.  ]. "end if"
].

"for -u option"
decompress := [ :outfile :infile |
|tree stack inBits ins count done treePtr outBit temp|
ins := FileStream open: 
      (Smalltalk arguments at: 2) mode: FileStream read.
inBits := BitStream new: ins.
outBit := OutBits new: outfile. 

stack := OrderedCollection new.  "stack of leaves/trees"
done := false. 

  "decoding tree, mark leaves with count of -1"
  [(inBits atEnd not) and: [done not]] whileTrue: [
    |nByte num nBit|

    nBit := (inBits nextBit).

    (nBit = 1) ifTrue: [
      (stack size >= 2) ifTrue: [
        |leftL rightR|
        rightR := stack removeLast. 
        leftL := stack removeLast.
        stack addLast: 
      (Tree new: (leftL char) count: 0 left: leftL right: rightR).  
      ] ifFalse: [
        tree := stack removeLast. 
        done := true. 
      ]. 
    ].
    nBit = 0 ifTrue: [
      num := 8. 
      nByte := 0. 
      [num > 0] whileTrue: [
        nBit := inBits nextBit. 
        nByte := nByte bitAt: num put: nBit. 
        num := num - 1. 
      ]. 
      nByte = 0 ifTrue: [
      (inBits nextBit) = 1 ifTrue: [
          nByte := nByte + EOF].
      ].
      stack add: (Leaf new: nByte count: -1).
    ].
  ].

  "now, decode"
  treePtr := tree.

  "write out corresponding bits, if 0 go left, if 1 go right"
  "continue looping with inBits"
    [inBits atEnd] whileFalse:[
    nBit := inBits nextBit. 

    nBit = 0 ifTrue: [
      tree := tree getLeft.

      (tree count) = -1 ifTrue: [ 
        temp := (tree char). 
        count := 8. 

        [count > 0] whileTrue: [
        outBit writeBit: (temp bitAt: count).
        count := count - 1.  
        ].
        tree := treePtr.
      ].
    ].
    nBit = 1 ifTrue: [
      tree := tree getRight.
        (tree count) = -1 ifTrue: [
        temp := (tree char). 
        count := 8. 

        [count > 0] whileTrue: [
        outBit writeBit: (temp bitAt: count). 
        count := count - 1. 
        ].
        tree := treePtr. 
      ]. 
    ].
  ].
].

"main block that calls other blocks"
main := [ 
    |infile args|
    args := (Smalltalk arguments at: 1).
    infile := FileStream open: 
      (Smalltalk arguments at: 2) mode: FileStream read.

    "check input arguments"
    (args includes: $d ) ifTrue: [ argd := true ]. 
    (args includes: $t ) ifTrue: [ argt := true ].
    (args includes: $c ) ifTrue: [ argc := true ].
    (args includes: $u ) ifTrue: [ argu := true ].

    "load freqtable"
    readInFile value: freqtable value: infile.
    "For EOF"
    freqtable incr: EOF. 

  "check command line arg for outfile"
  (Smalltalk arguments size) = 3 
  ifTrue: [
   |outfile| 
    outfile := FileStream open: 
    (Smalltalk arguments at: 3) mode: FileStream write.
    (argc or: [argt]) ifTrue: 
      [compress value: outfile value: infile. ].
    argu ifTrue: [decompress value: outfile value: infile.]. 
    outfile close.
  ] ifFalse: [
    (argc or: [argt]) ifTrue: [compress value: stdout value: infile].
    argu ifTrue: [decompress value: stdout value: infile]. 
  ].

  "success"
  infile close.  
  ObjectMemory quit: 0.
].

main value. 
