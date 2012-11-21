:- module(context_pdf_workflow, []).

/** <module> Context model workflow for PDF
    * classes
    * format
    * characteristic
*/

:- context:register(context_pdf_workflow:model_format).
:- context:register(context_pdf_workflow:model_characteristic).
:- context:register(context_pdf_workflow:model_values).
:- context:register(context_pdf_workflow:model_index).


model_index(_) :-
   reply_html_page(
     cliopatria(default),
     title('domains'),
     [h1('Domain Feature'),
      form([action('model_format')], % target('format')],
           [
            select([name('domain')],
               [option([value('slopes')],['terrain slopes']),
                option([value('wind')],  [wind]),
                option([value('lakes')], ['lake sizes']),
                option([value('particles')],['particle sizes']),
                option([value('rain')],  [rainfall]),
                option([value('clutter')],[clutter])]
            ),
            br([]),
            input([type('radio'),name('query_type'),value('pdf')]),b(pdf),
            input([type('radio'),name('query_type'),value('sample')]),b(sample),
            br([]),
            input([type('submit'), value('Select Domain')])
           ])
      ]
     ).


model_format(Request) :-
   http_parameters(Request, [domain(Domain),
                             query_type(Query)
                             ],
                             [attribute_declarations(rdf_load:param)]),
   Query = pdf,
   reply_html_page(
     cliopatria(default),
     title('format'),
     [h1('Format'),
      p(['domain: ', b(Domain)]),
      p(['query:  ', b(Query)]),
      form([action('model_characteristic')], % target('characteristic')],
           [
            select([name('format')],
               [option([value('html')],['HTML table']),
                option([value('xml')],  [xml]),
                option([value('json')], [json]),
                option([value('graph')],[graph]),
                option([value('fmi')],  [fmi])]
            ),
            br([]),
            input([type('checkbox'),name('cdf'),value(true)]),b('Cumulative'),
            input([type('hidden'),name(domain),value(Domain)]),
            input([type('hidden'),name(query_type),value(Query)]),
            input([type('hidden'),name(distribution),value(exponential)]),
            br([]),
            input([type('submit'), value('Select PDF Format')])
           ])

      ]
     ).

model_format(Request) :-
   http_parameters(Request, [domain(Domain),
                             query_type(Query)
                             ],
                             [attribute_declarations(param)]),
   Query = sample,
   reply_html_page(
     cliopatria(default),
     title('format'),
     [h1('Format'),
      p(['domain: ', b(Domain)]),
      p(['query:  ', b(Query)]),
      form([action('model_characteristic')], % target('characteristic')],
           [
            select([name('sampling')],
               [option([value('single')],['single value']),
                option([value('range')], ['range values'])]
            ),
            input([type('hidden'),name(domain),value(Domain)]),
            input([type('hidden'),name(query_type),value(Query)]),
            input([type('hidden'),name(distribution),value(exponential)]),
            br([]),
            input([type('submit'), value('Select Sampling Format')])
           ])

      ]
     ).


model_characteristic(Request) :-
   http_parameters(Request, [domain(Domain),
                             query_type(Query),
                             cdf(CDF),
                             format(Format),
                             distribution(Distribution)
                             ],
                             [attribute_declarations(param)]),
   Query = pdf,
   reply_html_page(
     cliopatria(default),
     title('Characteristic'),
     [table([border=0, width('100%'), height('600')],
	    [tr([td([width('20%'),height('100%'), valign(top)],
							       [
      h1('Characteristic'),
      p(['domain:       ', b(Domain)]),
      p(['query:        ', b(Query)]),
      p(['format:       ', b(Format)]),
      p(['cumulative:   ', b(CDF)]),
      p(['distribution: ', b(Distribution)]),
      form([action('/context_demos/gross_terrain'), target('results')],
           [
            input([type('hidden'),name(query_type),value(Query)]),
            input([type('hidden'),name(mean),value(1.0)]),
            input([type('hidden'),name(area_scale),value(local)]),
            input([type('hidden'),name(utm),value("none")]),
            input([type('hidden'),name(seed),value(1.0)]),
            br([]),
            input([type('submit'), value('Generate Distribution')])
           ])
				]),
			     td([iframe([name(results),
					 width('100%'),
					 height('100%')
					],
					[])])
			    ]
			   )

			]
	   )
      ]
     ).


model_characteristic(Request) :-
   http_parameters(Request, [domain(Domain),
                             query_type(Query),
                             sampling(Sampling),
                             distribution(Distribution)
                             ],
                             [attribute_declarations(param)]),
   Query = sample,
   reply_html_page(
     cliopatria(default),
     title('Characteristic'),
       [table([border=0, width('100%'), height('600')],
	      [tr([td([width('20%'),height('100%'), valign(top)],
		      [h1('Characteristic'),
		       p(['domain:       ', b(Domain)]),
		       p(['query:        ', b(Query)]),
		       p(['sampling:     ', b(Sampling)]),
		       p(['distribution: ', b(Distribution)])
		      ]
		     ),
		   td([iframe([name(results),
			       width('100%'),
			       height('100%')
			      ],
			      [])])
		  ]
		 )
	      ]
	     )
       ]
     ).
