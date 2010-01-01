{application, helloworld,
 [{description, "helloworld"},
  {vsn, "0.1"},
  {modules, [
    helloworld,
    helloworld_app,
    helloworld_sup,
    helloworld_deps,
    helloworld_resource,
    webmachine_mod,
    webmachine_config
  ]},
  {registered, []},
  {mod, {helloworld_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
