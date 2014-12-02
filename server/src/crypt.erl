-module(crypt).

-export([new/0, set_client_key/2, set_file_key/2, encrypt/2, decrypt/2]).

-include("config.hrl").

new() ->
    KeyRing = dict:new(),
    set_server_key(KeyRing).

set_server_key(KeyRing) ->
    ServerKey = get_server_key_arg(),
    dict:store(k1,ServerKey,KeyRing).

get_server_key_arg() ->
    {ok, [[ServerKeyPhrase, SSize]]} = init:get_argument(server_key),
    {Size,_} = string:to_integer(SSize),
    create_key(ServerKeyPhrase,Size).
    
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
