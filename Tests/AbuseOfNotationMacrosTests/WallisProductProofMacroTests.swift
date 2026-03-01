import SwiftSyntaxMacros
import SwiftSyntaxMacrosTestSupport
import XCTest

#if canImport(AbuseOfNotationMacros)
import AbuseOfNotationMacros

nonisolated(unsafe) let wallisProductProofMacros: [String: Macro.Type] = [
    "WallisProductProof": WallisProductProofMacro.self,
]
#endif

final class WallisProductProofMacroTests: XCTestCase {
    #if canImport(AbuseOfNotationMacros)

    func testDepthOne() throws {
        // Depth 1: W_0 = 1/1, W_1 = 4/3
        // Factor correspondence: (2*1-1)(2*1+1) + 1 = (2*1)^2, i.e. 3 + 1 = 4
        assertMacroExpansion(
            """
            @WallisProductProof(depth: 1)
            enum WallisProof {}
            """,
            expandedSource: """
            enum WallisProof {

                typealias _M1x0 = TimesZero<AddOne<Zero>>

                typealias _M1x1 = TimesSucc<_M1x0, PlusSucc<PlusZero<Zero>>>

                typealias _M1x2 = TimesSucc<_M1x1, PlusSucc<PlusZero<AddOne<Zero>>>>

                typealias _M1x3 = TimesSucc<_M1x2, PlusSucc<PlusZero<AddOne<AddOne<Zero>>>>>

                typealias _M2x0 = TimesZero<AddOne<AddOne<Zero>>>

                typealias _M2x1 = TimesSucc<_M2x0, PlusSucc<PlusSucc<PlusZero<Zero>>>>

                typealias _M2x2 = TimesSucc<_M2x1, PlusSucc<PlusSucc<PlusZero<AddOne<AddOne<Zero>>>>>>

                typealias _W0 = WallisBase

                typealias _W1 = WallisStep<_W0, _M1x2, _M2x2, _M1x1, _M1x3>

                typealias _WFC1 = PlusSucc<PlusZero<AddOne<AddOne<AddOne<Zero>>>>>

                func _wallisFactorCheck() {
                    assertEqual(_WFC1.Total.self, AddOne<AddOne<AddOne<AddOne<Zero>>>>.self)
                }
            }
            """,
            macros: wallisProductProofMacros
        )
    }

    func testZeroProducesDiagnostic() throws {
        assertMacroExpansion(
            """
            @WallisProductProof(depth: 0)
            enum WallisProof {}
            """,
            expandedSource: """
            enum WallisProof {}
            """,
            diagnostics: [
                DiagnosticSpec(message: "#wallisProductProof requires an integer literal >= 1", line: 1, column: 1)
            ],
            macros: wallisProductProofMacros
        )
    }

    #endif
}
