#ifndef HALVM_KERNEL_SYS_CAPABILITY_H
#define HALVM_KERNEL_SYS_CAPABILITY_H

struct __user_cap_header_struct {
  int version;
  int pid;
};

typedef struct __user_cap_header_struct cap_user_header_t;

struct __user_cap_data_struct {
  int effective;
  int permitted;
  int inheritable;
};

typedef struct __user_cap_data_struct cap_user_data_t;

#endif
