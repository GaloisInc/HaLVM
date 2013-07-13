// BANNERSTART
// - Copyright 2006-2008, Galois, Inc.
// - This software is distributed under a standard, three-clause BSD license.
// - Please see the file LICENSE, distributed with this software, for specific
// - terms and conditions.
// Author: Adam Wick <awick@galois.com>
// BANNEREND
//
#include <string.h>
#include <stdio.h>
#include <libIVC.h>
#include <xen/io/ring.h>
#include <sys/mman.h>
#include <openssl/aes.h>
#include <openssl/sha.h>

#define ENCRYPTION_REQRESP      0xEEEE
#define DECRYPTION_REQRESP      0xDDDD
#define HASH_REQRESP            0xABCD

#define PROT_RW (PROT_READ | PROT_WRITE)
#define PACKED __attribute__((__packed__))

typedef struct PACKED {
  u_int16_t kind;
  union {
    struct PACKED {
      u_int16_t gref;
      u_int32_t size;
      unsigned char key[16];
    } crypt;
    struct PACKED {
      u_int16_t gref;
      u_int32_t size;
    } hash;
  } u;
} cryptdev_request_t;

typedef struct PACKED {
  u_int16_t kind;
  union {
    struct PACKED {
      int32_t resp;
    } crypt;
    struct PACKED {
      int32_t resp;
      unsigned char hash[20];
    } hash;
  } u;
} cryptdev_response_t;

void do_cryption_request(unsigned long dom,
                         cryptdev_request_t *req, 
                         cryptdev_response_t *resp,
                         int enc)
{
  void *buffer = xc_gnttab_map_grant_ref(xcg, dom, req->u.crypt.gref, PROT_RW);
  unsigned char ivec[16];
  AES_KEY key;
  void *temp;
  int res;

  bzero(ivec, 16);
  if(!buffer) {
    resp->u.crypt.resp = -1;
    return;
  }

  // Adjust the size to be a multiple of 16
  resp->u.crypt.resp = (req->u.crypt.size + 15) & ~15;
  if(resp->u.crypt.resp > 4096) {
    resp->u.crypt.resp = -2;
    return;
  } else temp = malloc(resp->u.crypt.resp);

  res = enc == AES_ENCRYPT ? AES_set_encrypt_key(req->u.crypt.key, 128, &key)
                           : AES_set_decrypt_key(req->u.crypt.key, 128, &key);
  if(res) {
    resp->u.crypt.resp = -3;
    return;
  }

  AES_cbc_encrypt(buffer, temp, resp->u.crypt.resp, &key, ivec, enc);
  memcpy(buffer, temp, resp->u.crypt.resp);

  free(temp);
  xc_gnttab_munmap(xcg, buffer, 1);
}

void do_encryption_request(unsigned long dom,
                           cryptdev_request_t *req, 
                           cryptdev_response_t *resp)
{
  do_cryption_request(dom, req, resp, AES_ENCRYPT);
}

void do_decryption_request(unsigned long dom,
                           cryptdev_request_t *req, 
                           cryptdev_response_t *resp)
{
  do_cryption_request(dom, req, resp, AES_DECRYPT);
}

void do_hash_request(unsigned long dom,
                     cryptdev_request_t *req, 
                     cryptdev_response_t *resp)
{
  void *buffer = xc_gnttab_map_grant_ref(xcg, dom, req->u.hash.gref, PROT_RW);
  u_int32_t size = req->u.hash.size;

  if(!buffer) {
    resp->u.hash.resp = -1;
    return;
  }

  bzero(resp->u.hash.hash, sizeof(resp->u.hash.hash));
  if(SHA1(buffer, size, resp->u.hash.hash))
    resp->u.hash.resp = size;
  else
    resp->u.hash.resp = -2;
}

int main(int argc, char **argv)
{
  inout_chan *chan;

  initialize_libIVC_library();
  chan = connect_two_way("c_crypt_device");
  while(1) {
    cryptdev_request_t *req;
    cryptdev_response_t resp;
    int len = read_unknown_chan(chan, (void*)&req);

    if(!len) {
      printf("DEV: Error reading from input channel!\n");
      abort();
    }

    bzero(&resp, sizeof(resp));
    resp.kind = req->kind;
    switch(resp.kind) {
      case ENCRYPTION_REQRESP:
        do_cryption_request(channel_peer(chan), req, &resp, AES_ENCRYPT);
        len = 6;
        break;
      case DECRYPTION_REQRESP:
        do_cryption_request(channel_peer(chan), req, &resp, AES_DECRYPT);
        len = 6;
        break;
      case HASH_REQRESP:
        do_hash_request(channel_peer(chan), req, &resp);
        len = 26;
        break;
    }
    write_chan(chan, &resp, len);
  } 

  return 0;
}
