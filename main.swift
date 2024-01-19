indirect enum BoolOperation {
    case value(Bool)

    case variable(Character)

    //1st
    case not(BoolOperation) // one arg
    // 2nd
    case and(BoolOperation, BoolOperation) // double arg
    // 3rd
    case or(BoolOperation, BoolOperation)
    case xor(BoolOperation, BoolOperation)
    case implies(BoolOperation, BoolOperation)
    // 4th
    case equals(BoolOperation, BoolOperation)
    case notEquals(BoolOperation, BoolOperation)
}
var symbolDictionary : [String : Character] = [
  "and" : "∧",
  "not" : "¬",
  "or" : "∨",
  "xor" : "⊕",
  "imply" : "→",
  "equals" : "=",
  "notequals" : "≠",
  "true": "T",
  "false": "F"
]


typealias DoubleArgFunction = (BoolOperation, BoolOperation) -> BoolOperation
let doubleFunctionDict: [Character: DoubleArgFunction] = [
  "∧": { (lhs, rhs) in return .and(lhs, rhs) },
  "∨": { (lhs, rhs) in return .or(lhs, rhs) },
  "⊕": { (lhs, rhs) in return .xor(lhs, rhs) },
  "→": { (lhs, rhs) in return .implies(lhs, rhs) },
  "=": { (lhs, rhs) in return .equals(lhs, rhs) },
  "≠": { (lhs, rhs) in return .notEquals(lhs, rhs) }
]

let priorityDict: [Character: Int] = [ // default is 0
  "T": 0b00001,
  "F": 0b00001,
  "∧": 0b00010,
  "∨": 0b00100,
  "⊕": 0b00100,
  "→": 0b00100,
  "=": 0b01000,
  "≠": 0b01000
]

let doubleArgScan = doubleFunctionDict
  .map { (character, function) in (character, function, priorityDict[character]) }
  .filter { (_, _, priority) in priority != nil }
  .map { (character, function, priority) in (character, function, priority!) } // wah
  .sorted { lhs, rhs in // Some werid limitation in tupples getting destructured.
      let (_, _, lhsPriority) = lhs
      let (_, _, rhsPriority) = rhs
      return lhsPriority > rhsPriority
  }

enum GroupingBalance: Error {
    case noOpening
    case noClosing
}

func checkBalance(_ string: String) throws {
    var stack: [Character] = []

    for char in string {

        if char == "(" {
            stack.append(char)
        }

        if char == ")" {
            guard stack.popLast() != nil else {
                throw GroupingBalance.noOpening
            }
        }
    }

    guard stack.count == 0 else {
        throw GroupingBalance.noClosing
    }
}

func parse(_ formula: String) throws -> BoolOperation {

    func createNode(_ string: inout String) throws -> BoolOperation {
        try checkBalance(string) // Something went wrong when trying to break it down.

        let trimmedString = string.trimming()

        if (trimmedString.canBeUngrouped()) {
            let stringCpy = string
            string = trimmedString.slice(from: 1, through: trimmedString.count - 1)

            do { try checkBalance(string); return try createNode(&string) } catch {
                string = stringCpy
            }
        }

        var grouping = 0
        var ungroupedStartingIndex = 0

        for index in (0 ..< string.count) {
            guard let character = string.getCharacterAt(index) else {
                fatalError("No character at \(index)")
            }

            if character == ")" {
                grouping -= 1
            }
            if character == "(" {
                grouping += 1
            }

            if grouping != 0 || character == " " {
                continue
            }

            ungroupedStartingIndex = index

            break
        }

        for (functionCharacter, function, _) in doubleArgScan {
            for index in (ungroupedStartingIndex ..< string.count) {
                guard let character = string.getCharacterAt(index) else {
                    fatalError("No character at \(index)")
                }

                if character != functionCharacter {
                    continue
                }

                var leftSlice = string.slice(from: 0, through: index - 1)

                var rightSlice = string.slice(from: index + 1, through: string.count)

                guard let leftNode = try? createNode(&leftSlice) else {
                    fatalError("\(functionCharacter) is missing argument #1")
                }

                guard let rightNode = try? createNode(&rightSlice) else {
                    fatalError("\(functionCharacter) is missing argument #2")
                }

                return function(leftNode, rightNode)
            }
        }

        // TODO: remove some repition

        for index in (ungroupedStartingIndex ..< string.count) {
            guard let character = string.getCharacterAt(index) else {
                fatalError("No character at \(index)")
            }

            if character != "¬" {
                continue
            }

            var rightSlice = string.slice(from: index + 1, through: string.count)

            guard let rightNode = try? createNode(&rightSlice) else {
                fatalError("¬ is missing argument #1 `\(rightSlice)`")
            }

            return .not(rightNode)
        }

        if trimmedString.count == 1, let character = trimmedString.first  { // TODO: impl T/F
            return .variable(character)
        }

        fatalError("Failed to create node with `\(string)`")
    }

    var copy = formula

    return try createNode(&copy)
}

func findVariables(operations: BoolOperation) -> [(Character, Bool)] {
    var variables = [(Character, Bool)]()
    
    func explore(operations: BoolOperation) {
        switch operations {
        case .value, .not:
            break // Do nothing for leaf nodes or not operations
        case .variable(let variable):
            variables.append((variable, false))
        case .and(let lhs, let rhs),
             .or(let lhs, let rhs),
             .xor(let lhs, let rhs),
             .implies(let lhs, let rhs),
             .equals(let lhs, let rhs),
             .notEquals(let lhs, let rhs):
            explore(operations: lhs)
            explore(operations: rhs)
        }
    }
    explore(operations: operations)
    return variables
}

func compute(operations: BoolOperation, variables: [Character: Bool]) -> Bool {
    switch operations {
    case .value(let value):
        return value
    case .variable(let variable):
        return variables[variable] ?? false
    case .not(let operand):
        return !compute(operations: operand, variables: variables)
    case .and(let lhs, let rhs):
        return compute(operations: lhs, variables: variables) && compute(operations: rhs, variables: variables)
    case .or(let lhs, let rhs):
        return compute(operations: lhs, variables: variables) || compute(operations: rhs, variables: variables)
    case .xor(let lhs, let rhs):
        return compute(operations: lhs, variables: variables) != compute(operations: rhs, variables: variables)
    case .implies(let lhs, let rhs):
        return !compute(operations: lhs, variables: variables) || compute(operations: rhs, variables: variables)
    case .equals(let lhs, let rhs):
        return compute(operations: lhs, variables: variables) == compute(operations: rhs, variables: variables)
    case .notEquals(let lhs, let rhs):
        return compute(operations: lhs, variables: variables) != compute(operations: rhs, variables: variables)
    }
}

while var input = readLine() {
    for (key, value) in symbolDictionary {
        input = input.replacing(key, with: "\(value)")
    }

    let parsedFormula = try parse(input)
    let variables = findVariables(operations: parsedFormula)

    print(compute(operations: parsedFormula, variables: [:]))
    
}

extension Character {
    func isGrouping() -> Bool {
        return self == "(" || self == ")"
    }

    func getGrouping() -> Character? {
        return isGrouping() ? self : nil
    }
}

extension String {

    func canBeUngrouped() -> Bool {
        if let firstGrouping = self.first?.getGrouping(), let lastGrouping = self.last?.getGrouping() {
            return firstGrouping != lastGrouping
        }

        return false
    }

    func trimming() -> String {
        var nonWhitespaceStart: Int? = nil
        var nonWhitespaceEnd: Int? = nil

        for x in (0 ..< self.count) {
            if let character = self.getCharacterAt(x), !character.isWhitespace {
                if nonWhitespaceStart == nil {
                    nonWhitespaceStart = x
                }

                nonWhitespaceEnd = x + 1
            }
        }

        return slice(from: nonWhitespaceStart ?? 0, through: nonWhitespaceEnd ?? self.count)
    }

    func getCharacterAt(_ characterIndex: Int) -> Character? {
        if characterIndex > self.count {
            return nil
        }

        return self[self.index(self.startIndex, offsetBy: characterIndex)]
    }

    func slice(from: Int, through: Int) -> String {
        precondition(from < through, "Slice cannot create empty strings.")
        precondition(through <= self.count, "Through out of bounds for String slice.")

        let startIndex = self.index(self.startIndex, offsetBy: from)
        let endIndex = self.index(self.startIndex, offsetBy: through)

        return String(self[startIndex ..< endIndex])
    }
}
