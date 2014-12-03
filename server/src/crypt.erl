-module(crypt).

-export([new/0, set_client_key/2, set_file_key/2, encrypt/2, decrypt/2]).
-export([testing_des/1, testing_des3/1]).

-include("config.hrl").

new() ->
    KeyRing = dict:new(),
    set_server_key(KeyRing).

set_server_key(KeyRing) ->
    ServerKey = get_server_key_arg(),
    dict:store(k1,ServerKey,KeyRing).

get_server_key_arg() ->
    {ok, [[ServerKeyPhrase]]} = init:get_argument(server_key),
    create_key(ServerKeyPhrase,8).
    
set_client_key(ClientKey, KeyRing) ->
    dict:store(k2,ClientKey, KeyRing).

set_file_key(FileKey, KeyRing) ->
    dict:store(k3, FileKey, KeyRing).

encrypt(PlainText, KeyRing) ->
    Keys = dict:to_list(KeyRing),
    IVec = create_key(?MAJOR_VERSION,8),
    encrypt(PlainText, Keys, IVec).

encrypt(PlainText, [{k1,K1}], IVec) ->
    block_encrypt(des_cbc, K1, IVec, PlainText);
encrypt(PlainText, [{k1,K1},{k2,K2}], IVec) ->
    block_encrypt(des3_cbc, [K1,K2,K1], IVec, PlainText);
encrypt(PlainText, [{k1, K1},{k2,K2},{k3,K3}], IVec) ->
    block_encrypt(des3_cbc, [K1,K2,K3], IVec, PlainText).
    
block_encrypt(Type, Key, IVec, PlainText) ->
    ?log_heavydebug(["Encrypting ... \r\nPlainText=", PlainText, "\r\nWith type=",Type," Key=",Key]),
    crypto:block_encrypt(Type, Key, IVec, PlainText).


decrypt(CipherText, KeyRing) ->
    Keys = dict:to_list(KeyRing),
    IVec = create_key(?MAJOR_VERSION,8),
    decrypt(CipherText, Keys, IVec).

decrypt(CipherText, [{k1, K1}], IVec) ->
    block_decrypt(des_cbc, K1, IVec, CipherText);
decrypt(CipherText, [{k1, K1},{k2,K2}], IVec) ->
    block_decrypt(des3_cbc, [K1,K2,K1], IVec, CipherText);
decrypt(CipherText, [{k1,K1},{k2,K2},{k3,K3}], IVec) ->
    block_decrypt(des3_cbc, [K1,K2,K3], IVec, CipherText).

block_decrypt(Type, Key, IVec, CipherText) ->
    ?log_heavydebug(["Decrypting ...\r\nCipherText=", CipherText, "\r\nWith type=",Type," Key=",Key]),
    crypto:block_decrypt(Type, Key, IVec, CipherText).

create_key(Phrase,N) ->
    Data = phrase_convert(Phrase,phrase_init(Phrase,N)),
    create_key(Data, N,<<>>).

create_key(_, 0,Key) -> Key;
create_key(Data, N, Key) ->
    KB = key_block(Data),
    create_key(Data bsr 8, N-1, <<Key/binary, KB/binary>>).

key_block(D) ->
    <<D:1, (D bsr 1):1, (D bsr 2):1, (D bsr 3):1, (D bsr 4):1, (D bsr 5):1, (D bsr 6):1, (D bsr 7):1>>.

phrase_init([],Num) -> Num;
phrase_init([H|T], Num) -> 
    phrase_init(T, H+Num).

phrase_convert([], Num) -> Num;
phrase_convert([H|T],Num) ->
    phrase_convert(T, (H*Num + H)).

%% Only for testing the DES

testing_des(Size) ->
    KeyRing = new(),
    testing(KeyRing, Size).

testing_des3(Size) ->
    KeyRing = set_file_key(create_key("Filename",8),set_client_key(create_key("Client",8),new())),
    testing(KeyRing,Size).

testing(KeyRing, Size) ->
    ?log_info(["Building PlainText"]),
    Text = build("A",Size,""),
    ?log_info(["Encrypting data ..."]),
    C = encrypt(Text,KeyRing),
    ?log_info(["Calculating frequency"]),
    GB = gb_init(gb_trees:empty(),lists:seq(0,255)),
    Freq = freq(binary:bin_to_list(C), GB),
    ?log_info(["Printing result"]),
    print_gb(gb_trees:to_list(Freq),0,Size/256).

build(_,0,Text) -> Text;
build([T],N,Text) ->
    build([T],N-1,[T|Text]).

gb_init(GB,[]) -> GB;
gb_init(GB, [H|T]) ->
    gb_init(gb_trees:enter(H,0,GB),T).

freq([], GB) -> GB;
freq([H|T], GB) ->
    N = gb_trees:get(H,GB),
    freq(T,gb_trees:enter(H,N+1,GB)).

print_gb([],Diff,S) ->
    Avg = Diff/265,
    P = (Avg/S)*100,
    io:format("Total diff: ~p, average ~p (~p %), S=~p~n",[Diff, Avg, P, S]);
print_gb([{H,N}|T],D,S) ->
    Diff = if N<S -> S-N;
	    true -> N-S
	   end,
    io:format("~p: ~p (~p)~n",[H,N,Diff]),
    print_gb(T,D+Diff,S).
