{application, kad,
  [{description, "erlang kademlia dht library"},
   {id, "alpha version"},
   {vsn, "0.1"},
   {modules, [
   	      kad,
	      kad_api,
	      kad_coordinator,
	      kad_net,
	      kad_node,
	      kad_protocol,
	      kad_routing,
	      kad_rpc_mgr,
	      kad_searchlist,
	      kad_store,
	      kad_util
	      ]},
   {registered, [kad_sup, kad_node]},
   {application, [kernel, stdlib, sasl, crypto, inets]},
   {mod, {kad, []}},
   {env, [{ip, "127.0.0.1"}]}
   ]}.
