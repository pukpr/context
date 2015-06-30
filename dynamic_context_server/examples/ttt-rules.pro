%% strength rules for finding solutions on Tic-Tac-Toe, ways for x to win

strength(ul,ul_x,L,1.0000) :- member(uc_x, L), member(ur_x, L), !.
strength(ul,ul_x,L,0.0000) :- member(ml_x, L), member(ll_x, L).
strength(ul,ul_x,L,0.0000) :- member(mc_x, L), member(lr_x, L).
strength(ul,ul_x,_,0).
strength(ul,ul_o,_,0).
strength(uc,uc_x,L,1.0000) :- member(mc_x, L), member(lc_x, L), !.
strength(uc,uc_x,L,0.0000) :- member(ul_x, L), member(ur_x, L).
strength(uc,uc_x,_,0).
strength(uc,uc_o,_,0).
strength(ur,ur_x,L,1.0000) :- member(mr_x, L), member(lr_x, L), !.
strength(ur,ur_x,L,0.0000) :- member(ul_x, L), member(uc_x, L).
strength(ur,ur_x,L,0.0000) :- member(mc_x, L), member(ll_x, L).
strength(ur,ur_x,_,0).
strength(ur,ur_o,_,0).
strength(ml,ml_x,L,1.0000) :- member(ul_x, L), member(ll_x, L), !.
strength(ml,ml_x,L,0.0000) :- member(mc_x, L), member(mr_x, L).
strength(ml,ml_x,_,0).
strength(ml,ml_o,_,0).
strength(mc,mc_x,L,0.0000) :- member(ur_x, L), member(ll_x, L).
strength(mc,mc_x,L,0.0000) :- member(ul_x, L), member(lr_x, L).
strength(mc,mc_x,L,0.0000) :- member(ml_x, L), member(mr_x, L).
strength(mc,mc_x,L,0.0000) :- member(uc_x, L), member(lc_x, L).
strength(mc,mc_x,_,0).
strength(mc,mc_o,_,0).
strength(mr,mr_x,L,1.0000) :- member(ml_x, L), member(mc_x, L), !.
strength(mr,mr_x,L,0.0000) :- member(ur_x, L), member(lr_x, L).
strength(mr,mr_x,_,0).
strength(mr,mr_o,_,0).
strength(ll,ll_x,L,1.0000) :- member(mc_x, L), member(ur_x, L), !.
strength(ll,ll_x,L,0.0000) :- member(lc_x, L), member(lr_x, L).
strength(ll,ll_x,L,0.0000) :- member(ml_x, L), member(ul_x, L).
strength(ll,ll_x,_,0).
strength(ll,ll_o,_,0).
strength(lc,lc_x,L,1.0000) :- member(ll_x, L), member(lr_x, L), !.
strength(lc,lc_x,L,0.0000) :- member(uc_x, L), member(mc_x, L).
strength(lc,lc_x,_,0).
strength(lc,lc_o,_,0).
strength(lr,lr_x,L,1.0000) :- member(ul_x, L), member(mc_x, L), !.
strength(lr,lr_x,L,0.0000) :- member(mr_x, L), member(ur_x, L).
strength(lr,lr_x,L,0.0000) :- member(ll_x, L), member(lc_x, L).
strength(lr,lr_x,_,0).
strength(lr,lr_o,_,0).
