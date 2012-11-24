% implicit attribute declarations for http_parameters processing

:- multifile
   param/2.

param(domain, [oneof([slopes,wind,lakes,particles,rain,clutter,none]),
               default(none)]).
param(cdf, [boolean,
            default(false)]).
param(format, [oneof([html,xml,json,graph,fmi,none]),
               default(none)]).
param(sampling, [oneof([single, range]),
                 default(single)]).
param(distribution, [oneof([exponential, stretched, fat, none]),
                     default(none)]).

param(mean, [float]).
param(area_scale, [oneof(['local','wide'])]).
param(query_type, [oneof(['pdf',  'sample']),
                   default('pdf')]).
param(utm, [optional(true)]).
param(seed, [optional(true)]).
param(plot_scaling, [oneof([log, lin]),
                     default(log)]).
param(plot_type, [oneof([dynamic, scatter]),
                     default(dynamic)]).
param(file_name, []).
param(data_set, []).
param(characteristic, [profile, psd]).



