
% Simple example illustrating how one _might_ generate wrappers for
% the Chandra X-Ray Center DataModel library.

slirp_define_opaque("dmDataset");

slirp_define_opaque("dmBlock");

slirp_define_opaque("dmDescriptor");
slirp_map_ref("dmDescriptor**");

% Ideally one would not want to _require_ the S-Lang coder to have to
% call functions like dmFilterFree(), dmBlockFree() etc themselves,
% but since it appears that the DM does not prevent one from freeing,
% say a dmBuffer*, twice, it would be dangerous to use a mapping like
%
%	slirp_define_opaque("dmBuffer","dmFilterFree");
%
% to have SLIRP generate code to automatically free the dmBuffer
% without _also_ telling SLIRP to _not_ generate a wrapper for
% dmFilterFree().
%
% Since this is the kind of decision that only the actual DM library
% binder can make, this example SLIRP rc file does not install a 
% "freer" cleanup func for the dmBuffer opaque type.
%
% Ditto for regRegion and the other aggregate/opaque types.

slirp_define_opaque("dmBuffer");

slirp_define_opaque("regRegion");

slirp_map_int("dmErrCode");
slirp_map_int("dmDataType");
slirp_map_int("dmBool");
