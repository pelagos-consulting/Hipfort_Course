
    call hipcheck(hipmemcpy(B_h, B_d, size(B_d), hipmemcpydevicetohost))