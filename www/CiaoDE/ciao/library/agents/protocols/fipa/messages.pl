%%--------------------------------------------------%%
%%  FIPA Communicative Act Library Specification    %%
%%--------------------------------------------------%%

:- message_def [performative, sender, receiver, reply_to, content,
    language, encoding, ontology, protocol, conversation_id, reply_with,
    in_reply_to, reply_by].

:- message accept_proposal.
:- message agree.
:- message cancel.
:- message cfp.
:- message confirm.
:- message disconfirm.
:- message failure.
:- message inform.
:- message not_understood.
:- message propagate.
:- message propose.
:- message proxy.
:- message query_if.
:- message query_ref.
:- message refuse.
:- message proposal.
:- message request.
:- message request_when.
:- message request_whenever.
:- message subscribe.
