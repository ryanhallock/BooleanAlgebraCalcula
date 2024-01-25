import Foundation

indirect enum BoolOperation {
    case value(Bool)

    case variable(String)

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
  "implies" : "→",
  "equals" : "=",
  "notequals" : "≠",
  "true": "⊤",
  "false": "⊥",
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

func parseUngrouped(formula string: String) -> [Range<Int>] {
    var returnRange: [Range<Int>] = []

    var grouping = 0
    var currentRange: Range<Int>? = nil
    for index in (0 ..< string.count) {
        guard let character = string.getCharacterAt(index) else {
                fatalError("No character at \(index)")
        }

        switch character {
        case ")":
            grouping -= 1
        case "(":
            if let range = currentRange, grouping == 0 {
                returnRange.append(range.lowerBound ..< index)
                currentRange = nil
            }

            grouping += 1
        default:
            if grouping == 0 && currentRange == nil {
                currentRange = (index ..< string.count)
            }
        }
    }

    if let currentRange = currentRange { // Swift should auto-unwrap nils if checked.
        returnRange.append(currentRange)
    }

    return returnRange
}

func parse(_ formula: String) throws -> BoolOperation {

    func createNode(_ string: inout String) throws -> BoolOperation {
        try checkBalance(string) // Something went wrong when trying to break it down.

        let trimmedString = string.trimming()

        if (trimmedString.canBeUngrouped()) {
            let stringCpy = string
            string = trimmedString.slice(from: 1, to: trimmedString.count - 1)

            do {
                try checkBalance(string)
                return try createNode(&string)
            } catch {
                string = stringCpy
            }
        }

        let ungroupedRanges = parseUngrouped(formula: string)

        for (functionCharacter, function, _) in doubleArgScan {
            for range in ungroupedRanges {
                for index in range {
                    guard let character = string.getCharacterAt(index) else {
                        fatalError("No character at \(index)")
                    }

                    if character != functionCharacter {
                        continue
                    }

                    var leftSlice = string.slice(from: 0, to: index - 1)

                    var rightSlice = string.slice(from: index + 1, to: string.count)

                    guard let leftNode = try? createNode(&leftSlice) else {
                        fatalError("\(functionCharacter) is missing argument #1")
                    }

                    guard let rightNode = try? createNode(&rightSlice) else {
                        fatalError("\(functionCharacter) is missing argument #2")
                    }

                    return function(leftNode, rightNode)
                }
            }
        }

        // TODO: remove some repition
        for range in ungroupedRanges {
            for index in range {
                guard let character = string.getCharacterAt(index) else {
                    fatalError("No character at \(index)")
                }

                if character != "¬" {
                    continue
                }

                var rightSlice = string.slice(from: index + 1, to: string.count)

                guard let rightNode = try? createNode(&rightSlice) else {
                    fatalError("¬ is missing argument #1 `\(rightSlice)`")
                }

                return .not(rightNode)
            }
        }

        guard !trimmedString.contains(" ") || (trimmedString.first == "`" && trimmedString.last == "`") else { // Conditions with innerspaces are formatted wrong. but ignore `test alpha`
            fatalError("Failed to create node with `\(string)`")
        }

        switch trimmedString {
        case "⊤":
            return .value(true)
        case "⊥":
            return .value(false)
        default:
            return .variable(trimmedString)
        }
    }

    var copy = formula

    return try createNode(&copy)
}

func findVariables(operations: BoolOperation) -> [String] {
    var variables: [String] = []
    
    func explore(operations: BoolOperation) {
        switch operations {
        case .value:
            break // Do nothing for leaf nodes
        case .variable(let variable):
            variables.append(variable)
        case .not(let rhs):
            explore(operations: rhs)
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

func compute(operations: BoolOperation, variables: [String: Bool]) -> Bool {
    switch operations {
    case .value(let value):
        return value
    case .variable(let variable):
        guard let value = variables[variable] else {
            fatalError("Variable \(variable) does not have a value.")
        }
        return value
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
        input = input.replacingOccurrences(of: key, with: "\(value)", options: .caseInsensitive)
    }

    let parsedFormula = try parse(input)

    let characterVaraibles = findVariables(operations: parsedFormula)

    let outputRowCount = Int(pow(2, Double(characterVaraibles.count))) // workaround because swift lacks a basic pow function

    var rows: [RowData] = []

    for rowValue in (0..<outputRowCount) {
        var variables: [String: Bool] = [:]

        for variableIndex in (0..<characterVaraibles.count) {
            let variable = characterVaraibles[variableIndex]

            // Reverse order the default truth table that is made with shifting.
            let maxIndex = characterVaraibles.count - 1
            let value = (rowValue & (1 << (maxIndex - variableIndex))) != 0

            variables[variable] = value
        }

        let finalValue = compute(operations: parsedFormula, variables: variables)

        rows.append((variables, finalValue))
    }

    printPrettyTable(formula: input, variables: characterVaraibles, rows: rows)
}

// Row stores variable truth/false and the result after the function is run.
typealias RowData = ([String: Bool], Bool)

//Gpted
func printPrettyTable(formula: String, variables: [String], rows: [RowData]) {
    let redPrefix = "\u{001B}[0;31m"
    let greenPrefix = "\u{001B}[0;32m"
    let resetSuffix = "\u{001B}[0;0m"

    // Print table header
    print("｜ ", terminator: "")
    for variable in variables {
        print("\(variable) ｜ ", terminator: "")
    }
    print("\(formula) ｜")

    // Print table separator
    let separatorLength = (variables.count * 5) + (formula.count) + 5
    print(String(repeating: "-", count: separatorLength))

    // Print table rows
    for (variablesValues, value) in rows {
        print("｜", terminator: "")
        for variable in variables {
            guard let value = variablesValues[variable] else {
                fatalError("Missing variable \(variable) in output")
            }
            print(" \(value ? "\(greenPrefix)T\(resetSuffix)" : "\(redPrefix)F\(resetSuffix)") ｜", terminator: "")
        }

        let result = value ? "\(greenPrefix)T\(resetSuffix)" : "\(redPrefix)F\(resetSuffix)"
        let padding = (formula.count - 1) / 2  // Adjust padding for centering
        print(" \(String(repeating: " ", count: padding))\(result)\(String(repeating: " ", count: padding)) ｜")
    }

    print(String(repeating: "-", count: separatorLength))
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

        return slice(from: nonWhitespaceStart ?? 0, to: nonWhitespaceEnd ?? self.count)
    }

    func getCharacterAt(_ characterIndex: Int) -> Character? {
        if characterIndex > self.count {
            return nil
        }

        return self[self.index(self.startIndex, offsetBy: characterIndex)]
    }

    func slice(from: Int, to: Int) -> String {
        precondition(from < to, "Slice cannot create empty strings.")
        precondition(to <= self.count, "Through out of bounds for String slice.")

        let startIndex = self.index(self.startIndex, offsetBy: from)
        let endIndex = self.index(self.startIndex, offsetBy: to)

        return String(self[startIndex ..< endIndex])
    }
}
