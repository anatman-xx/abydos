{application, areasrv,
    [{description, "The Area Server"},
     {vsn, "1.0"},
     {modules, [areasrv_app, areasrv_sup, areasrv, area]},
     {registered, [areasrv, areasrv_sup]},
     {applications, [kernel, stdlib]},
     {mod, {areasrv_app, []}},
     {start_phases, []},
     {env, [
        {monsrv, ['monsrv@client']},
        {charsrv, 'charsrv@client'},
        {libs_auto_load, [libstd.sup, libplayer.sup, libenv.sup,
            libtree.sup, libid.sup, libsave.sup]}
     ]}
    ]}.

