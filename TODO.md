# gen_rpc TODO

This is a list of pending features or code technical debt for `gen_rpc`:

- Implement SSL connectivity, including CN-based authentication
- Implement per-id-and-node tuple connection sharing to spread workload on multiple mailboxes per node
- Add module versioning functionality to fail calls on out of date modules
- Add heartbeat support to detect broken connections
