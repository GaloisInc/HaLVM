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
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <libIVC.h>
#include <xen/xen.h>
#include <xen/io/ring.h>
#include <xen/event_channel.h>
#include <xenctrl.h>
#include <sys/mman.h>
#include <openssl/aes.h>
#include <openssl/sha.h>

#define ENCRYPTION_REQRESP      0xeeeeeeee
#define DECRYPTION_REQRESP      0xdddddddd
#define HASH_REQRESP            0xabcddcba

#define PROT_RW (PROT_READ | PROT_WRITE)

typedef struct {
  uint32_t kind;
  uint32_t id;
  uint32_t gref;
  uint32_t size;
  uint8_t  aes_key[16];
} cryptdev_request_t;

typedef struct {
  uint32_t kind;
  uint32_t id;
  int32_t  resp;
  uint8_t  sha1_hash[20];
} cryptdev_response_t;

struct client_data {
  uint32_t dom;
  evtchn_port_t port;
  void *page;
};

void do_cryption_request(struct client_data *cdata,
                         cryptdev_request_t *req,
                         cryptdev_response_t *resp,
                         int enc)
{
  void *buffer = xc_gnttab_map_grant_ref(xcg, cdata->dom, req->gref, PROT_RW);
  unsigned char ivec[16];
  AES_KEY key;
  void *temp;
  int res;

  bzero(ivec, 16);
  if(!buffer) {
    resp->resp = -1;
    return;
  }

  // Adjust the size to be a multiple of 16
  resp->resp = (req->size + 15) & ~15;
  if(resp->resp > 4096) {
    resp->resp = -2;
    return;
  } else temp = malloc(resp->resp);

  res = enc == AES_ENCRYPT ? AES_set_encrypt_key(req->aes_key, 128, &key)
                           : AES_set_decrypt_key(req->aes_key, 128, &key);
  if(res) {
    resp->resp = -3;
    return;
  }

  AES_cbc_encrypt(buffer, temp, resp->resp, &key, ivec, enc);
  memcpy(buffer, temp, resp->resp);

  free(temp);
  xc_gnttab_munmap(xcg, buffer, 1);
}

void do_encryption_request(struct client_data *cdata,
                           cryptdev_request_t *req, 
                           cryptdev_response_t *resp)
{
  do_cryption_request(cdata, req, resp, AES_ENCRYPT);
}

void do_decryption_request(struct client_data *cdata,
                           cryptdev_request_t *req, 
                           cryptdev_response_t *resp)
{
  do_cryption_request(cdata, req, resp, AES_DECRYPT);
}

void do_hash_request(struct client_data *cdata,
                     cryptdev_request_t *req, 
                     cryptdev_response_t *resp)
{
  void *buffer = xc_gnttab_map_grant_ref(xcg, cdata->dom, req->gref, PROT_RW);
  u_int32_t size = req->size;

  if(!buffer) {
    resp->resp = -1;
    return;
  }

  bzero(resp->sha1_hash, sizeof(resp->sha1_hash));
  if(SHA1(buffer, size, resp->sha1_hash))
    resp->resp = size;
  else
    resp->resp = -2;
}

DEFINE_RING_TYPES(cryptdev, cryptdev_request_t, cryptdev_response_t);

static void backend_handler(void *void_data)
{
  struct client_data *cdata = (struct client_data *)void_data;
  cryptdev_back_ring_t back_ring;

  printf ("DEV: backend_handler start\n");

  BACK_RING_INIT(&back_ring, (cryptdev_sring_t *)cdata->page, 4096);
  printf ("DEV: BACK_RING_INIT\n");

  while(1) {
    if(RING_HAS_UNCONSUMED_REQUESTS(&back_ring)) {
      int more_to_do = 1;
      cryptdev_request_t *request;
      cryptdev_response_t *response;
      RING_IDX rc, rp;
      int notify = 0;

      while(more_to_do) {
        rc = back_ring.req_cons;
        rp = back_ring.sring->req_prod;
        more_to_do = 0;
        rmb();

        while(rc != rp) {
          if(RING_REQUEST_CONS_OVERFLOW(&back_ring, rc))
            break;

          request = RING_GET_REQUEST(&back_ring, rc);
          response = (cryptdev_response_t*)request;
          switch(request->kind) {
            case ENCRYPTION_REQRESP:
              do_encryption_request(cdata, request, response); 
              break;
            case DECRYPTION_REQRESP:
              do_decryption_request(cdata, request, response);
              break;
            case HASH_REQRESP:
              do_hash_request(cdata, request, response);
              break;
            default:
              printf("UNKNOWN REQUEST CODE %x\n", request->kind);
              break;
          }
          back_ring.rsp_prod_pvt++;
          back_ring.req_cons=++rc;
        }

        RING_PUSH_RESPONSES_AND_CHECK_NOTIFY(&back_ring, notify);
        if(back_ring.rsp_prod_pvt == back_ring.req_cons) {
          RING_FINAL_CHECK_FOR_REQUESTS(&back_ring, more_to_do);
        } else more_to_do = 1;
        if(notify) xc_evtchn_notify(xce, cdata->port);
      }
    } else {
      while(xc_evtchn_pending(xce) != cdata->port) {}
      if(xc_evtchn_unmask(xce, cdata->port) < 0)
        printf("Error unmasking event channel!\n");
    }
  }
}

int main(int argc, char **argv)
{
  xen_backend *backend;

  initialize_libIVC_library();
  printf("DEV: Initialized libIVC\n");

  //SSL_library_init();
  backend = create_responder("simple_openssl_device");
  if(!backend) {
    printf("DEV: Failed to build backend listener!\n");
    return -1;
  }
  printf("DEV: backend listener alive!\n");

  while(1) {
    struct client_data *cdata = malloc(sizeof(struct client_data));

    if (!cdata) {
      printf("DEV: couldn't malloc client data\n");
      break;
    }
    else {
      printf("DEV: malloc okay\n");
    }

    printf("DEV: waiting for connection from client ...");
    accept_connection(backend, (unsigned long*)&cdata->dom,
                      &cdata->page, &cdata->port);
    printf("connected!\n");
    if(fork()) {
      // parent thread, run the backend handler.
      printf("DEV: running handler\n");
      backend_handler(cdata);
    } else {
      printf("DEV: loop and wait for next request\n");
      // child thread, continue on.
    }
  }

  printf("DEV: main thread quitting\n");
  return 0;
}
