:- module(context_emi, [
			    %  fxn1/3,
			    %  fxn2/3,
			    %  fxn3/3
			    ]).

/** <module> Knowledge for EMI modeling
    * Indexing to EMI modeling documents
    *
*/

:- use_module(context_math).

:- context:register(context_emi:navigate).
:- context:register(context_emi:plot).

% /ref/cable_EMI_susceptibility.pdf#nameddest=CS_BCI
%%   emi_doc(-Doc)
%
%    returns the main EMI doc
emi_doc('foundation/cable_EMI_susceptibility.pdf').

%%   ref(+Key, -Contents)//
%
%    Inline references a pointer to a document from a key
ref(Key, Contents) -->
    {
     emi_doc(FileName),
     context:ref_link_to_pdf(FileName, Key, Link)
    },
    html(p([
	     \(con_text:gif(ref)),
             a([href=Link,target=target_iframe,
               type='application/pdf'], Contents)
	   ]
	  )
	).


%%   navigate(+Request) 
%
%    Dynamic page to EMI models and documentation
navigate(Request) :-
   reply_html_page(cliopatria(default),
                   [title('general EMI models')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1(a(href('navigate'),'Links to EMI models')),
                      p('Linked specifications provided for EMI models'),

                      h2('Electromagnetic environments, externally stimulated'),
	             \(context_emi:ref('RF_EME','Radiated Radio Frequency (RF) Electromagnetic Environment (RF EME)')),
	             \(context_emi:ref('EMP','Electromagnetic Pulse (EMP)')),
	             \(context_emi:ref('LE','Lightning Effects')),
	             \(context_emi:ref('HPM','High Power Microwave')),
	             \(context_emi:ref('ED','Electrostatic Discharge')),
	             \(context_emi:ref('IGEE','Internally Generated Electromagnetic Energy')),

                      h2('Induced EM environment for internal equipment'),
                      p('The following are internal design considerations'),

                      h3('Conducted Susceptibility'),
	             \(context_emi:ref('CS_RF','RF')),
	             \(context_emi:ref('CS_BCI','Bulk Cable Injection, Impulses')),
	             \(context_emi:ref('CS_DST','Damped Sinusoidal Transients')),

                      h3('Radiated Susceptibility'),
	             \(context_emi:ref('RS_EF','Electric Field')),
	             \(context_emi:ref('RS_TSEF','EMP')),

                      br([]),
		      \(con_text:render_iframe(render))
                     ]
						       ))
                   ]
                  ).












