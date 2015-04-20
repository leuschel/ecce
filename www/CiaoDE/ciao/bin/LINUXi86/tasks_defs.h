void reinit_list(goal_descriptor_p goal);
void init_goal_desc_list(void);
/*int kill_thread(goal_descriptor_p goal_to_kill);*/
void allow_thread_cancellation(void);
void disallow_thread_cancellation(void);
goal_descriptor_p attach_me_to_goal_desc_list(Argdecl);
void associate_wam_goal(Argdecl, goal_descriptor_p goal_desc);
void print_task_status(Argdecl);
void make_goal_desc_free(goal_descriptor_p goal);
goal_descriptor_p init_first_gd_entry(void);
goal_descriptor_p gimme_a_new_gd(void);
goal_descriptor_p look_for_a_free_goal_desc(void);
struct worker *get_my_worker(void);
void enqueue_thread(THREAD_T thread);
void unlink_wam(goal_descriptor_p goal);
