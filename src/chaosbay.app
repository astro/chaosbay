{application, chaosbay,
 [{description, "chaosbay"},
  {vsn, "0.01"},
  {modules, [
    chaosbay,
    chaosbay_app,
    chaosbay_sup,
    chaosbay_web,
    chaosbay_deps
  ]},
  {registered, []},
  {mod, {chaosbay_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
