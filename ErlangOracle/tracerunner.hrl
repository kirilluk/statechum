
-record(statechum, {processNum,compiledModules=sets:new(),config = dict:new(), attr = dict:new()}).

-define(erlCoverage,'erlCoverage').
-define(erlFlushDelay,'erlFlushDelay').
