:- module(context_workflow, []).

/** <module> Context workflow
    * Link to PDF workflow
    * Link to PSD workflow
    * Link to deterministic profile workflow
*/

:- context:register(context_workflow:navigate).

%%   navigate(+Request)
%
%    Dynamic page to workflows
navigate(Request) :-
   reply_html_page(cliopatria(default),
                   [title('Workflow navigate')],
      [
         \(con_text:table_with_iframe_target(
                        Request,
                        [
                         h2('Workflows available'),
                         ul([
                             li([\(con_text:gif(workflow)),
                                 \(con_text:alert(['Invoke the ', b('OSCAR'), ' workflow'],
                                                  'Not linked yet'))]),
                             li([\(con_text:gif(workflow)),
                                 a(href('/context_profile/navigate'),
                                   'Invoke a workflow to use profiled terrains')]),
                             li([\(con_text:gif(workflow)),
                                 a(href('/context_psd_workflow/navigate'),
                                   'Invoke a workflow to access model PSD artifacts')]),
                             li([\(con_text:gif(workflow)),
                                 a(href('/context_obstacles/navigate'),
                                   'Invoke a workflow to use obstacle artifacts')]),
                             li([\(con_text:gif(workflow)),
                                 a([href='/context_model/navigate'],
                                   'Invoke workflow to access PDF models')]),
                             li([\(con_text:gif(workflow)),
                                 'Example workflow artifact:'])
                            ]),
                         \(context_pdf:example)
                        ]
                                            )
          )
      ]
                  ).










