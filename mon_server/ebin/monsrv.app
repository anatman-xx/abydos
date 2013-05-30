{application, monsrv,
    [{description, "The Monitor Server"},
     {vsn, "1.0"},
     {modules, [monsrv_app, monsrv_sup, monsrv, 
        mon]},
     {registered, [monsrv, monsrv_sup]},
     {applications, [kernel, stdlib]},
     {mod, {monsrv_app, []}},
     {start_phases, []},
     {env, [
        {mem_check_nodes, ['connsrv@client', 
            'accsrv@client', 'charsrv@client',
            'underworld@client']}
        ]}
    ]}.

