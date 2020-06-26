
/* functions registered to make them visible to .Call() in R. */
extern SEXP all_equal_bn(SEXP, SEXP);
extern SEXP allsubs_test(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP alpha_star(SEXP, SEXP, SEXP);
extern SEXP amat2arcs(SEXP, SEXP);
extern SEXP aracne(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP arcs_cg_assumptions(SEXP, SEXP, SEXP);
extern SEXP arcs_rbind(SEXP, SEXP, SEXP);
extern SEXP arcs2amat(SEXP, SEXP);
extern SEXP arcs2elist(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bn_recovery(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bootstrap_arc_coefficients(SEXP, SEXP);
extern SEXP bootstrap_reduce(SEXP);
extern SEXP bootstrap_strength_counters(SEXP, SEXP, SEXP, SEXP);
extern SEXP cache_partial_structure(SEXP, SEXP, SEXP, SEXP);
extern SEXP cache_structure(SEXP, SEXP, SEXP);
extern SEXP castelo_completion(SEXP, SEXP, SEXP);
extern SEXP ccgpred(SEXP, SEXP, SEXP, SEXP);
extern SEXP cdpred(SEXP, SEXP, SEXP, SEXP);
extern SEXP cg_banned_arcs(SEXP, SEXP);
extern SEXP cgpred(SEXP, SEXP, SEXP);
extern SEXP cgsd(SEXP, SEXP, SEXP);
extern SEXP chow_liu(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP class_err(SEXP, SEXP);
extern SEXP configurations(SEXP, SEXP, SEXP);
extern SEXP count_observed_values(SEXP);
extern SEXP cpdag(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP cpdist_lw(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP dag2ug(SEXP, SEXP, SEXP);
extern SEXP data_frame_finite(SEXP);
extern SEXP data_type(SEXP);
extern SEXP dataframe_column(SEXP, SEXP, SEXP);
extern SEXP dedup(SEXP, SEXP, SEXP, SEXP);
extern SEXP dpred(SEXP, SEXP, SEXP, SEXP);
extern SEXP elist2arcs(SEXP);
extern SEXP empty_graph(SEXP, SEXP);
extern SEXP entropy_loss(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP fast_cglm(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP fast_lm(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP fit2arcs(SEXP);
extern SEXP fitted_mb(SEXP, SEXP);
extern SEXP fitted_vs_data(SEXP, SEXP, SEXP);
extern SEXP get_test_counter();
extern SEXP gpred(SEXP, SEXP, SEXP);
extern SEXP has_pdag_path(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP hc_opt_step(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP hc_to_be_added(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP ide_cozman_graph(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP increment_test_counter(SEXP);
extern SEXP indep_test(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP is_dag(SEXP, SEXP);
extern SEXP is_listed(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP is_pdag_acyclic(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP is_row_equal(SEXP, SEXP);
extern SEXP lw_weights(SEXP, SEXP, SEXP, SEXP);
extern SEXP mappred(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP match_brace(SEXP, SEXP, SEXP, SEXP);
extern SEXP mean_strength(SEXP, SEXP, SEXP);
extern SEXP mi(SEXP, SEXP, SEXP, SEXP);
extern SEXP minimal_data_frame(SEXP);
extern SEXP minimal_table(SEXP, SEXP);
extern SEXP naivepred(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP nbr2arcs(SEXP);
extern SEXP normalize_cpt(SEXP);
extern SEXP nparams_cgnet(SEXP, SEXP, SEXP);
extern SEXP nparams_fitted(SEXP, SEXP, SEXP);
extern SEXP num_arcs(SEXP);
extern SEXP onLoad();
extern SEXP onUnload();
extern SEXP ordered_graph(SEXP, SEXP, SEXP);
extern SEXP pdag_extension(SEXP, SEXP, SEXP);
extern SEXP pdag2dag(SEXP, SEXP);
extern SEXP per_node_score(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rbn_master(SEXP, SEXP, SEXP, SEXP);
extern SEXP reset_test_counter();
extern SEXP root_nodes(SEXP, SEXP);
extern SEXP roundrobin_test(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP score_cache_fill(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP score_delta(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP shd(SEXP, SEXP, SEXP);
extern SEXP smart_network_averaging(SEXP, SEXP, SEXP);
extern SEXP subsets(SEXP, SEXP);
extern SEXP tabu_hash(SEXP, SEXP, SEXP, SEXP);
extern SEXP tabu_step(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP tiers(SEXP, SEXP);
extern SEXP topological_ordering(SEXP, SEXP, SEXP, SEXP);
extern SEXP tree_directions(SEXP, SEXP, SEXP, SEXP);
extern SEXP unique_arcs(SEXP, SEXP, SEXP);
extern SEXP vstructures(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP which_undirected(SEXP, SEXP);

