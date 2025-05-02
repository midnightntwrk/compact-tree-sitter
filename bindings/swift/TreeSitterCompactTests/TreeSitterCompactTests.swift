import XCTest
import SwiftTreeSitter
import TreeSitterCompact

final class TreeSitterCompactTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_compact())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Compact grammar")
    }
}
