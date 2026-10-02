import Concrete.Proof.DefinitionIdentity

/-!
# Generated attestation references — do not edit

Emitted by `scripts/gen/attestation_refs.sh` from the attestation manifest, which is itself
derived from compiler-produced subject facts. A proof author SELECTS one of these symbols; the
components are never written by hand.

`Except`-typed because `of?` validates and the constructor is private: a reference that fails
validation stays a refusal rather than becoming a value.
-/

namespace Concrete.Proof.GeneratedAttestations

def empty_0ccbc808_partial : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "0ccbc8088b15d1c95a8901cd5c389546" "states" "partial" "ecd934a117e4376625071651869df4c5"
def empty_0ccbc808_stale : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "0ccbc8088b15d1c95a8901cd5c389546" "states" "stale" "c850a160887198d750551e999bf5bc83"
def empty_194e5755_ch : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "194e5755f1e63c2b5b67b955aebf639e" "hmac_sha256" "ch" "3e315e17b9a1420dc0c9c4988af52511"
def empty_194e5755_sha256_init : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "194e5755f1e63c2b5b67b955aebf639e" "hmac_sha256" "sha256_init" "1cbbcac2dedb20c50ca91b1953ab562f"
def empty_241230cb_ch : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "241230cb9fadb24763d83fc58e421bc0" "hmac_sha256" "ch" "f58b438c3db9c748789c8de68945fdea"
def empty_651aaa47_scale_by_two : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "651aaa47b6160f99e32031bc41e534db" "workspace" "scale_by_two" "5dbfd1f078e081530352591407626e2c"
def empty_6a57372e_sum4 : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "6a57372ef2d8c03ab5d8e9d4e7acbb21" "fold" "sum4" "6b0b02cd3611f49788a6f88669115d5f"
def empty_7f57f485_copy2 : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "7f57f485aed0e37c0156fcd3c71523cf" "loopcopy" "copy2" "c8a37b314145120f678fcf80ca1d6d27"
def empty_940edbf5_plain : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "940edbf5e1ff4cccc660ec9a65e1e13f" "ghost" "plain" "7114bc0f7ce3e447aa9bd74d3e6d4418"
def empty_940edbf5_with_ghost : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "940edbf5e1ff4cccc660ec9a65e1e13f" "ghost" "with_ghost" "9fce59fc43e5787ce68c810e409cf38f"
def empty_a6f721da_put : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "a6f721da1cccb629e1ec10d0d19f914e" "arr" "put" "74839d8db744f8ea74fe51737d89c273"
def empty_a90ea7d6_add_three : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "a90ea7d649c3aad1ee60020e9dcbb6ee" "straight_line" "add_three" "dc6d724c16bea86f3f371ddf2162119b"
def empty_ec245e65_weak : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "ec245e65d646766585a1d2d4b440d3fd" "cn" "weak" "ff19cefd91e9ef7261e5d696e6228cad"
def cryptoFns_95c968e5_check_nonce : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "95c968e5065f5d92f1882a5e0e0f99d3" "main" "check_nonce" "0a3ca2002a335ffd62b6cd22edad6146"
def cryptoFns_95c968e5_compute_tag : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "95c968e5065f5d92f1882a5e0e0f99d3" "main" "compute_tag" "8fdb09fce36c5df91cdb5fee3c00e35d"
def cryptoFns_95c968e5_verify_message : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "95c968e5065f5d92f1882a5e0e0f99d3" "main" "verify_message" "424aecbf3aafee722fe95c69fb8b1c25"
def cryptoFns_95c968e5_verify_tag : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "95c968e5065f5d92f1882a5e0e0f99d3" "main" "verify_tag" "20035d4f9ef9b58229a6fa855cbfbaa3"
def cryptoFns_d6d04163_check_nonce : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "d6d04163b35aefaf1599b37dfa02fc9b" "main" "check_nonce" "0a3ca2002a335ffd62b6cd22edad6146"
def ctTagFns_13c8e415_ct_compare : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "13c8e4151b399c089fad1a4124686597" "constant_time_tag" "ct_compare" "93ab28850d90691e47ac5af7685e9185"
def ctTagFns_404dc2c1_ct_compare : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "404dc2c1dea3a3819db69464378db145" "demo" "ct_compare" "4d81ef861a6be6f536dae068b2052b79"
def ctTagFns_9daef0b4_ct_compare : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "9daef0b489d977567ce5da8fb0dbb05a" "constant_time_tag" "ct_compare" "18f6e83241d4e9da29cc8cd88d604da0"
def elfFns_0b4c54cf_check_class : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "0b4c54cf4070140024c1198aa4afc4e1" "main" "check_class" "e5a6873b3e7f463367da16bc68f2cbbf"
def elfFns_0b4c54cf_check_data : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "0b4c54cf4070140024c1198aa4afc4e1" "main" "check_data" "b8a148bbb74cfaf7852c74289a803990"
def elfFns_0b4c54cf_check_magic : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "0b4c54cf4070140024c1198aa4afc4e1" "main" "check_magic" "a774a2e10d4c812456c99912239a7a81"
def elfFns_0b4c54cf_check_version : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "0b4c54cf4070140024c1198aa4afc4e1" "main" "check_version" "b24dd48ad33452aac540f4fbd34f4908"
def elfFns_0b4c54cf_validate_header : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "0b4c54cf4070140024c1198aa4afc4e1" "main" "validate_header" "98a73d3ab40b6c8bff43aec13289c06a"
def fixedCapacityFns_44a81ba0_compute_tag : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "44a81ba0c74768f38ef0345167b06484" "fixed_capacity" "compute_tag" "5795ae892b334d8b71693973e8f594c5"
def fixedCapacityFns_44a81ba0_ring_contains : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "44a81ba0c74768f38ef0345167b06484" "fixed_capacity" "ring_contains" "a7edaad35c68e6208cda2a68675eabcc"
def fixedCapacityFns_44a81ba0_ring_new : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "44a81ba0c74768f38ef0345167b06484" "fixed_capacity" "ring_new" "32b00d8bfe666e62cbf0fc8b1f214aa5"
def fixedCapacityFns_44a81ba0_ring_push : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "44a81ba0c74768f38ef0345167b06484" "fixed_capacity" "ring_push" "0f72c306156fe4290388c7a40dede638"
def parseValidateFns_5b60e3b6_compute_checksum : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "5b60e3b6c9056f004ebb2fc0f5f25762" "parse_validate" "compute_checksum" "c721ca16b467550acd17663a705264ba"
def parseValidateFns_5b60e3b6_parse_header : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "5b60e3b6c9056f004ebb2fc0f5f25762" "parse_validate" "parse_header" "a18adda69efda6f636be5dc8bb3f8aeb"
def parseValidateFns_5b60e3b6_validate_checksum : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "5b60e3b6c9056f004ebb2fc0f5f25762" "parse_validate" "validate_checksum" "8ec0a8624d33f1d624f9c67b544221c0"
def parseValidateFns_5b60e3b6_validate_header_fields : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "5b60e3b6c9056f004ebb2fc0f5f25762" "parse_validate" "validate_header_fields" "94f8271c82b0833bb9b317f8342aa7f0"
def parseValidateFns_5b60e3b6_validate_msg_type : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "5b60e3b6c9056f004ebb2fc0f5f25762" "parse_validate" "validate_msg_type" "6cb688bcfaafa530ef3ef85942060cb2"
def parseValidateFns_5b60e3b6_validate_payload_len : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "5b60e3b6c9056f004ebb2fc0f5f25762" "parse_validate" "validate_payload_len" "4c20bb22a0c4d96a48ef4abcc4b0dd0f"
def parseValidateFns_5b60e3b6_validate_total_len : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "5b60e3b6c9056f004ebb2fc0f5f25762" "parse_validate" "validate_total_len" "867ba31c93068cc4a2e4f29d41424155"
def parseValidateFns_5b60e3b6_validate_version : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "5b60e3b6c9056f004ebb2fc0f5f25762" "parse_validate" "validate_version" "5eea51e813c69ef88321a943434cf957"
def shaFns_194e5755_big_sigma0 : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "194e5755f1e63c2b5b67b955aebf639e" "hmac_sha256" "big_sigma0" "2fc56fcc1fe1179b08d9634389f4e6da"
def shaFns_194e5755_big_sigma1 : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "194e5755f1e63c2b5b67b955aebf639e" "hmac_sha256" "big_sigma1" "bf9e5f67627630de3db89a6a505259bb"
def shaFns_194e5755_block_to_words : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "194e5755f1e63c2b5b67b955aebf639e" "hmac_sha256" "block_to_words" "2566f0877ae2ddb261ffb5ffe3d745ff"
def shaFns_194e5755_block_to_words_at : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "194e5755f1e63c2b5b67b955aebf639e" "hmac_sha256" "block_to_words_at" "db8c024f5788e80b30e6997a0e84f903"
def shaFns_194e5755_ch : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "194e5755f1e63c2b5b67b955aebf639e" "hmac_sha256" "ch" "3e315e17b9a1420dc0c9c4988af52511"
def shaFns_194e5755_hmac_sha256 : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "194e5755f1e63c2b5b67b955aebf639e" "hmac_sha256" "hmac_sha256" "3c91b6c0721b1532979b0ab5071d8115"
def shaFns_194e5755_maj : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "194e5755f1e63c2b5b67b955aebf639e" "hmac_sha256" "maj" "24c6e19e81df257bf73ad1a67b7d9d44"
def shaFns_194e5755_sha256_compress : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "194e5755f1e63c2b5b67b955aebf639e" "hmac_sha256" "sha256_compress" "db23b155922cb4251360f8f6ba63708d"
def shaFns_194e5755_sha256_compress_at : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "194e5755f1e63c2b5b67b955aebf639e" "hmac_sha256" "sha256_compress_at" "0419685e7ed4f0816654b1ddf3a67c43"
def shaFns_194e5755_sha256_hash : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "194e5755f1e63c2b5b67b955aebf639e" "hmac_sha256" "sha256_hash" "a75e1f52a094df0bfbbb0b3f5d9bb0e1"
def shaFns_194e5755_sha256_init : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "194e5755f1e63c2b5b67b955aebf639e" "hmac_sha256" "sha256_init" "1cbbcac2dedb20c50ca91b1953ab562f"
def shaFns_194e5755_sha256_k : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "194e5755f1e63c2b5b67b955aebf639e" "hmac_sha256" "sha256_k" "a857c18a4abd382142dd072913aa3e6f"
def shaFns_194e5755_sha256_round : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "194e5755f1e63c2b5b67b955aebf639e" "hmac_sha256" "sha256_round" "629f9b2876a77f0f5d52a860659daeb8"
def shaFns_194e5755_sha256_schedule : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "194e5755f1e63c2b5b67b955aebf639e" "hmac_sha256" "sha256_schedule" "b40af686e1947479a7135f852e1e0f36"
def shaFns_194e5755_small_sigma0 : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "194e5755f1e63c2b5b67b955aebf639e" "hmac_sha256" "small_sigma0" "e153ac437b8dbdd5315d8728fde99e3b"
def shaFns_194e5755_small_sigma1 : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "194e5755f1e63c2b5b67b955aebf639e" "hmac_sha256" "small_sigma1" "ebdbe65affa2c57d59b8906600aa02f4"
def shaFns_194e5755_state_to_bytes : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "194e5755f1e63c2b5b67b955aebf639e" "hmac_sha256" "state_to_bytes" "9744d6bcb60e617a310af956a8ba1b6a"
def combineFns_5b493da6_combine : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "5b493da61c43a31949917f69f37fbd04" "calls" "combine" "75e3325a189ef34014260cc046888b42"
def combineFns_5b493da6_inc : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "5b493da61c43a31949917f69f37fbd04" "calls" "inc" "697ea521ed81fbf8df819cc45438561d"
def combineFns_6f99408c_combine : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "6f99408c80246883cc5821f5976cefd4" "calls" "combine" "75e3325a189ef34014260cc046888b42"
def combineFns_6f99408c_dbl : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "6f99408c80246883cc5821f5976cefd4" "calls" "dbl" "c3f254410bdebc8d14050f5087629bf0"
def combineFns_6f99408c_inc : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "6f99408c80246883cc5821f5976cefd4" "calls" "inc" "697ea521ed81fbf8df819cc45438561d"
def combineFns_f795a8cd_combine : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "f795a8cd7fcfbf94d1852740d0c79a6d" "calls" "combine" "75e3325a189ef34014260cc046888b42"
def combineFns_f795a8cd_dbl : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "f795a8cd7fcfbf94d1852740d0c79a6d" "calls" "dbl" "c3f254410bdebc8d14050f5087629bf0"
def combineFns_f795a8cd_inc : Except DefinitionIdentityRefusal DefinitionIdentity :=
  DefinitionIdentity.of? "f795a8cd7fcfbf94d1852740d0c79a6d" "calls" "inc" "697ea521ed81fbf8df819cc45438561d"

end Concrete.Proof.GeneratedAttestations
