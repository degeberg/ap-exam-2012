import Test.QuickCheck

import SoilAst
import SoilParser

-- Test Programs
helloWorld :: String
helloWorld = "let #print() from message = \n  send (message) to #println\nend\n\ncreate #hw1 with #print()\ncreate #hw2 with #print()\nsend (#Hello) to #hw1\nsend (#World) to #hw2"

cleanUp :: String
cleanUp = "let #dub() from message  = \n  case message of\n    (sender, msg) : send (self, msg) to sender\n                    send (self, msg) to sender\n    _             : send (#FaultyMessage) to #println \n  end\nend\n\nlet #half(state) from message = \n  if state == #skip then\n    become #half(#return)\n    send (#SkippingMessage) to #println\n  else\n    case message of\n      (sender, msg) : become #half(#skip)\n                      send (self, msg) to sender\n      _             : send (#FaultyMessage) to #println \n    end\n  end\nend\n    \ncreate #dubproc with #dub()\ncreate #halfproc with #half(#return)\nsend (#halfproc, #foo) to #dubproc"

gateKeeper :: String
gateKeeper = "let #printer () from message = \n  send (message) to #println\nend\n\nlet #gate (fst, fstmsg) from message = \n  case message of\n    (snd, sndmsg) : \n      if fst == #none then\n        become #gate(snd, sndmsg)\n      else\n        send (fstmsg, sndmsg) to fst concat snd\n        become #gate(#none, #none)\n      end\n    _ : \n  end\nend\n\nlet #repeat (other) from message = \n  send (message) to #gatekeeper\n  send (message) to other\nend\n  \ncreate #foobar with #printer()\ncreate #gatekeeper with #gate(#none, #none)\ncreate #repeater1 with #repeat(#repeater2)\ncreate #repeater2 with #repeat(#repeater1)\nsend (#foo, #Hello) to #repeater1\nsend (#bar, #World) to #repeater2\nsend (#foo, #Bye) to #repeater1\n"

-- ASTs

astHelloWorld :: Program
astHelloWorld = ([Func {funcname = "print", params = [], receive = "message", body = Acts [SendTo [Par "message"] (Id "println")]}],[Create (Id "hw1") (Id "print") [],Create (Id "hw2") (Id "print") [],SendTo [Id "Hello"] (Id "hw1"),SendTo [Id "World"] (Id "hw2")])

astCleanUp :: Program
astCleanUp = ([Func {funcname = "dub", params = [], receive = "message", body = CaseOf (Par "message") [(["sender","msg"],Acts [SendTo [Self,Par "msg"] (Par "sender"),SendTo [Self,Par "msg"] (Par "sender")])] (Acts [SendTo [Id "FaultyMessage"] (Id "println")])},Func {funcname = "half", params = ["state"], receive = "message", body = IfEq (Par "state") (Id "skip") (Acts [Become (Id "half") [Id "return"],SendTo [Id "SkippingMessage"] (Id "println")]) (CaseOf (Par "message") [(["sender","msg"],Acts [Become (Id "half") [Id "skip"],SendTo [Self,Par "msg"] (Par "sender")])] (Acts [SendTo [Id "FaultyMessage"] (Id "println")]))}],[Create (Id "dubproc") (Id "dub") [],Create (Id "halfproc") (Id "half") [Id "return"],SendTo [Id "halfproc",Id "foo"] (Id "dubproc")])

astGateKeeper :: Program
astGateKeeper = ([Func {funcname = "printer", params = [], receive = "message", body = Acts [SendTo [Par "message"] (Id "println")]},Func {funcname = "gate", params = ["fst","fstmsg"], receive = "message", body = CaseOf (Par "message") [(["snd","sndmsg"],IfEq (Par "fst") (Id "none") (Acts [Become (Id "gate") [Par "snd",Par "sndmsg"]]) (Acts [SendTo [Par "fstmsg",Par "sndmsg"] (Concat (Par "fst") (Par "snd")),Become (Id "gate") [Id "none",Id "none"]]))] (Acts [])},Func {funcname = "repeat", params = ["other"], receive = "message", body = Acts [SendTo [Par "message"] (Id "gatekeeper"),SendTo [Par "message"] (Par "other")]}],[Create (Id "foobar") (Id "printer") [],Create (Id "gatekeeper") (Id "gate") [Id "none",Id "none"],Create (Id "repeater1") (Id "repeat") [Id "repeater2"],Create (Id "repeater2") (Id "repeat") [Id "repeater1"],SendTo [Id "foo",Id "Hello"] (Id "repeater1"),SendTo [Id "bar",Id "World"] (Id "repeater2"),SendTo [Id "foo",Id "Bye"] (Id "repeater1")])

-- Tests

parses :: String -> Program -> Bool
parses s p = case parseString s of
               Left _   -> False
               Right p' -> p == p'

prop_test_helloWorld :: Bool
prop_test_helloWorld = parses helloWorld astHelloWorld

prop_test_cleanUp :: Bool
prop_test_cleanUp = parses cleanUp astCleanUp

prop_test_gateKeeper :: Bool
prop_test_gateKeeper = parses gateKeeper astGateKeeper

tests :: [(String, IO ())]
tests = [ ("helloWorld.soil parses correctly", quickCheck prop_test_helloWorld)
        , ("cleanUp.soil parses correctly", quickCheck prop_test_cleanUp)
        , ("gateKeeper.soil parses correctly", quickCheck prop_test_gateKeeper)
        ]

main :: IO ()
main = mapM_ (\(s, t) -> putStr (s ++ ": ") >> t) tests
