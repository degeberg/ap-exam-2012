import Test.QuickCheck

import SoilAst
import SoilInterp

-- ASTs

astHelloWorld :: Program
astHelloWorld = ([Func {funcname = "print", params = [], receive = "message", body = Acts [SendTo [Par "message"] (Id "println")]}],[Create (Id "hw1") (Id "print") [],Create (Id "hw2") (Id "print") [],SendTo [Id "Hello"] (Id "hw1"),SendTo [Id "World"] (Id "hw2")])

astCleanUp :: Program
astCleanUp = ([Func {funcname = "dub", params = [], receive = "message", body = CaseOf (Par "message") [(["sender","msg"],Acts [SendTo [Self,Par "msg"] (Par "sender"),SendTo [Self,Par "msg"] (Par "sender")])] (Acts [SendTo [Id "FaultyMessage"] (Id "println")])},Func {funcname = "half", params = ["state"], receive = "message", body = IfEq (Par "state") (Id "skip") (Acts [Become (Id "half") [Id "return"],SendTo [Id "SkippingMessage"] (Id "println")]) (CaseOf (Par "message") [(["sender","msg"],Acts [Become (Id "half") [Id "skip"],SendTo [Self,Par "msg"] (Par "sender")])] (Acts [SendTo [Id "FaultyMessage"] (Id "println")]))}],[Create (Id "dubproc") (Id "dub") [],Create (Id "halfproc") (Id "half") [Id "return"],SendTo [Id "halfproc",Id "foo"] (Id "dubproc")])

astGateKeeper :: Program
astGateKeeper = ([Func {funcname = "printer", params = [], receive = "message", body = Acts [SendTo [Par "message"] (Id "println")]},Func {funcname = "gate", params = ["fst","fstmsg"], receive = "message", body = CaseOf (Par "message") [(["snd","sndmsg"],IfEq (Par "fst") (Id "none") (Acts [Become (Id "gate") [Par "snd",Par "sndmsg"]]) (Acts [SendTo [Par "fstmsg",Par "sndmsg"] (Concat (Par "fst") (Par "snd")),Become (Id "gate") [Id "none",Id "none"]]))] (Acts [])},Func {funcname = "repeat", params = ["other"], receive = "message", body = Acts [SendTo [Par "message"] (Id "gatekeeper"),SendTo [Par "message"] (Par "other")]}],[Create (Id "foobar") (Id "printer") [],Create (Id "gatekeeper") (Id "gate") [Id "none",Id "none"],Create (Id "repeater1") (Id "repeat") [Id "repeater2"],Create (Id "repeater2") (Id "repeat") [Id "repeater1"],SendTo [Id "foo",Id "Hello"] (Id "repeater1"),SendTo [Id "bar",Id "World"] (Id "repeater2"),SendTo [Id "foo",Id "Bye"] (Id "repeater1")])

-- Tests

prop_test_helloWorldRR :: Bool
prop_test_helloWorldRR = runProgRR 20 astHelloWorld == (["World","Hello"],[])

prop_test_cleanUpRR :: Bool
prop_test_cleanUpRR = runProgRR 20 astCleanUp == (["SkippingMessage","SkippingMessage","SkippingMessage","SkippingMessage"],[])

prop_test_gateKeeperRR :: Bool
prop_test_gateKeeperRR = runProgRR 20 astGateKeeper == (["Hello:World"],["invalidFunction:foofoo"])

prop_test_helloWorldAll :: Bool
prop_test_helloWorldAll = False -- not implemented correctly

prop_test_cleanUpAll :: Bool
prop_test_cleanUpAll = False -- not implemented correctly

prop_test_gateKeeperAll :: Bool
prop_test_gateKeeperAll = False -- not implemented correctly

tests :: [(String, IO ())]
tests = [ ("helloWorld.soil interprets correctly with RR", quickCheck prop_test_helloWorldRR)
        , ("cleanUp.soil interprets correctly with RR", quickCheck prop_test_cleanUpRR)
        , ("gateKeeper.soil interprets correctly with RR", quickCheck prop_test_gateKeeperRR)
        , ("helloWorld.soil interprets correctly with All", quickCheck prop_test_helloWorldAll)
        , ("cleanUp.soil interprets correctly with All", quickCheck prop_test_cleanUpAll)
        , ("gateKeeper.soil interprets correctly with All", quickCheck prop_test_gateKeeperAll)
        ]

main :: IO ()
main = mapM_ (\(s, t) -> putStr (s ++ ": ") >> t) tests
