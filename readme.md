<div _ngcontent-quc-c228="" id="programming-exercise-instructions-content" class="guided-tour instructions__content__markdown markdown-preview"><h3 id="lagrange">Lagrange</h3>
<p>Given a set of points, polynomial interpolation is the task of finding a polynomial with
lowest degree that passes through all the points. Assume the set <span><span class="katex"><span class="katex-mathml"><math><mrow><mo>(</mo><msub><mi>x</mi><mn>0</mn></msub><mo separator="true">,</mo><msub><mi>y</mi><mn>0</mn></msub><mo>)</mo><mo separator="true">,</mo><mi mathvariant="normal">.</mi><mi mathvariant="normal">.</mi><mi mathvariant="normal">.</mi><mo separator="true">,</mo><mo>(</mo><msub><mi>x</mi><mi>n</mi></msub><mo separator="true">,</mo><msub><mi>y</mi><mi>n</mi></msub><mo>)</mo></mrow>{(x_0, y_0), . . . ,(x_n, y_n)}</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 1em; vertical-align: -0.25em;" class="strut"></span><span class="mord"><span class="mopen">(</span><span class="mord"><span class="mord mathdefault">x</span><span class="msupsub"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height: 0.301108em;" class="vlist"><span style="top: -2.55em; margin-left: 0em; margin-right: 0.05em;" class=""><span style="height: 2.7em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span class="mord mtight">0</span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height: 0.15em;" class="vlist"><span class=""></span></span></span></span></span></span><span class="mpunct">,</span><span style="margin-right: 0.166667em;" class="mspace"></span><span class="mord"><span style="margin-right: 0.03588em;" class="mord mathdefault">y</span><span class="msupsub"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height: 0.301108em;" class="vlist"><span style="top: -2.55em; margin-left: -0.03588em; margin-right: 0.05em;" class=""><span style="height: 2.7em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span class="mord mtight">0</span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height: 0.15em;" class="vlist"><span class=""></span></span></span></span></span></span><span class="mclose">)</span><span class="mpunct">,</span><span style="margin-right: 0.166667em;" class="mspace"></span><span class="mord">.</span><span class="mord">.</span><span class="mord">.</span><span class="mpunct">,</span><span style="margin-right: 0.166667em;" class="mspace"></span><span class="mopen">(</span><span class="mord"><span class="mord mathdefault">x</span><span class="msupsub"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height: 0.151392em;" class="vlist"><span style="top: -2.55em; margin-left: 0em; margin-right: 0.05em;" class=""><span style="height: 2.7em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span class="mord mathdefault mtight">n</span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height: 0.15em;" class="vlist"><span class=""></span></span></span></span></span></span><span class="mpunct">,</span><span style="margin-right: 0.166667em;" class="mspace"></span><span class="mord"><span style="margin-right: 0.03588em;" class="mord mathdefault">y</span><span class="msupsub"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height: 0.151392em;" class="vlist"><span style="top: -2.55em; margin-left: -0.03588em; margin-right: 0.05em;" class=""><span style="height: 2.7em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span class="mord mathdefault mtight">n</span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height: 0.15em;" class="vlist"><span class=""></span></span></span></span></span></span><span class="mclose">)</span></span></span></span></span></span> of
points. Then a suitable polynomial is given by</p>
<span title="L(x) := \sum_{j=0}^{n} y_jl_j(x)"><span class="katex-display"><span class="katex"><span class="katex-mathml"><math><mrow><mi>L</mi><mo>(</mo><mi>x</mi><mo>)</mo><mo>:</mo><mo>=</mo><munderover><mo>∑</mo><mrow><mi>j</mi><mo>=</mo><mn>0</mn></mrow><mi>n</mi></munderover><msub><mi>y</mi><mi>j</mi></msub><msub><mi>l</mi><mi>j</mi></msub><mo>(</mo><mi>x</mi><mo>)</mo></mrow>L(x) := \sum_{j=0}^{n} y_jl_j(x)
</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height:1em;vertical-align:-0.25em;" class="strut"></span><span class="mord mathdefault">L</span><span class="mopen">(</span><span class="mord mathdefault">x</span><span class="mclose">)</span><span style="margin-right:0.2777777777777778em;" class="mspace"></span><span class="mrel">:</span></span><span class="base"><span style="height:0.36687em;vertical-align:0em;" class="strut"></span><span class="mrel">=</span><span style="margin-right:0.2777777777777778em;" class="mspace"></span></span><span class="base"><span style="height:3.0651740000000007em;vertical-align:-1.4137769999999998em;" class="strut"></span><span class="mop op-limits"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height:1.6513970000000007em;" class="vlist"><span style="top:-1.872331em;margin-left:0em;"><span style="height:3.05em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span class="mord mtight"><span style="margin-right:0.05724em;" class="mord mathdefault mtight">j</span><span class="mrel mtight">=</span><span class="mord mtight">0</span></span></span></span><span style="top:-3.050005em;"><span style="height:3.05em;" class="pstrut"></span><span><span class="mop op-symbol large-op">∑</span></span></span><span style="top:-4.3000050000000005em;margin-left:0em;"><span style="height:3.05em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span class="mord mtight"><span class="mord mathdefault mtight">n</span></span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height:1.4137769999999998em;" class="vlist"><span></span></span></span></span></span><span style="margin-right:0.16666666666666666em;" class="mspace"></span><span class="mord"><span style="margin-right:0.03588em;" class="mord mathdefault">y</span><span class="msupsub"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height:0.311664em;" class="vlist"><span style="top:-2.5500000000000003em;margin-left:-0.03588em;margin-right:0.05em;"><span style="height:2.7em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span style="margin-right:0.05724em;" class="mord mathdefault mtight">j</span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height:0.286108em;" class="vlist"><span></span></span></span></span></span></span><span class="mord"><span style="margin-right:0.01968em;" class="mord mathdefault">l</span><span class="msupsub"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height:0.311664em;" class="vlist"><span style="top:-2.5500000000000003em;margin-left:-0.01968em;margin-right:0.05em;"><span style="height:2.7em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span style="margin-right:0.05724em;" class="mord mathdefault mtight">j</span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height:0.286108em;" class="vlist"><span></span></span></span></span></span></span><span class="mopen">(</span><span class="mord mathdefault">x</span><span class="mclose">)</span></span></span></span></span></span>
<p>where the Lagrange polynomials <span><span class="katex"><span class="katex-mathml"><math><mrow><msub><mi>l</mi><mi>j</mi></msub><mo>(</mo><mi>x</mi><mo>)</mo></mrow>l_j(x)</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 1.03611em; vertical-align: -0.286108em;" class="strut"></span><span class="mord"><span style="margin-right: 0.01968em;" class="mord mathdefault">l</span><span class="msupsub"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height: 0.311664em;" class="vlist"><span style="top: -2.55em; margin-left: -0.01968em; margin-right: 0.05em;" class=""><span style="height: 2.7em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span style="margin-right: 0.05724em;" class="mord mathdefault mtight">j</span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height: 0.286108em;" class="vlist"><span class=""></span></span></span></span></span></span><span class="mopen">(</span><span class="mord mathdefault">x</span><span class="mclose">)</span></span></span></span></span> are defined as follows:</p>
<span title="l_j(x) := \prod_{0 \leq k \leq n \\ \land k \;\mathrlap{\,/}{=}\; j} \frac{x - x_k}{x_j - x_k}"><span class="katex-display"><span class="katex"><span class="katex-mathml"><math><mrow><msub><mi>l</mi><mi>j</mi></msub><mo>(</mo><mi>x</mi><mo>)</mo><mo>:</mo><mo>=</mo><munder><mo>∏</mo><mrow><mn>0</mn><mo>≤</mo><mi>k</mi><mo>≤</mo><mi>n</mi><mspace></mspace><mo>∧</mo><mi>k</mi><mtext>  </mtext><mpadded width="0px"><mrow><mtext> </mtext><mi mathvariant="normal">/</mi></mrow></mpadded><mo>=</mo><mtext>  </mtext><mi>j</mi></mrow></munder><mfrac><mrow><mi>x</mi><mo>−</mo><msub><mi>x</mi><mi>k</mi></msub></mrow><mrow><msub><mi>x</mi><mi>j</mi></msub><mo>−</mo><msub><mi>x</mi><mi>k</mi></msub></mrow></mfrac></mrow>l_j(x) := \prod_{0 \leq k \leq n \\ \land k \;\mathrlap{\,/}{=}\; j} \frac{x - x_k}{x_j - x_k}
</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height:1.036108em;vertical-align:-0.286108em;" class="strut"></span><span class="mord"><span style="margin-right:0.01968em;" class="mord mathdefault">l</span><span class="msupsub"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height:0.311664em;" class="vlist"><span style="top:-2.5500000000000003em;margin-left:-0.01968em;margin-right:0.05em;"><span style="height:2.7em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span style="margin-right:0.05724em;" class="mord mathdefault mtight">j</span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height:0.286108em;" class="vlist"><span></span></span></span></span></span></span><span class="mopen">(</span><span class="mord mathdefault">x</span><span class="mclose">)</span><span style="margin-right:0.2777777777777778em;" class="mspace"></span><span class="mrel">:</span></span><span class="base"><span style="height:0.36687em;vertical-align:0em;" class="strut"></span><span class="mrel">=</span><span style="margin-right:0.2777777777777778em;" class="mspace"></span></span><span class="base"><span style="height:2.7763350000000004em;vertical-align:-1.516005em;" class="strut"></span><span class="mop op-limits"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height:1.050005em;" class="vlist"><span style="top:-1.808995em;margin-left:0em;"><span style="height:3.05em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span class="mord mtight"><span class="mord mtight">0</span><span class="mrel mtight">≤</span><span style="margin-right:0.03148em;" class="mord mathdefault mtight">k</span><span class="mrel mtight">≤</span><span class="mord mathdefault mtight">n</span><span class="mspace mtight newline"></span><span class="mbin mtight">∧</span><span style="margin-right:0.03148em;" class="mord mathdefault mtight">k</span><span style="margin-right:0.3252777777777778em;" class="mspace mtight"></span><span class="mord mtight"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height:0.75em;" class="vlist"><span style="top:-2.75em;"><span style="height:2.75em;" class="pstrut"></span><span class="rlap mtight"><span style="height:1em;vertical-align:-0.25em;" class="strut"></span><span class="inner"><span class="mord mtight"><span style="margin-right:0.19516666666666668em;" class="mspace mtight"></span><span class="mord mtight">/</span></span></span><span class="fix"></span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height:0.25em;" class="vlist"><span></span></span></span></span></span><span class="mord mtight"><span class="mrel mtight">=</span></span><span style="margin-right:0.3252777777777778em;" class="mspace mtight"></span><span style="margin-right:0.05724em;" class="mord mathdefault mtight">j</span></span></span></span><span style="top:-3.0500049999999996em;"><span style="height:3.05em;" class="pstrut"></span><span><span class="mop op-symbol large-op">∏</span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height:1.516005em;" class="vlist"><span></span></span></span></span></span><span style="margin-right:0.16666666666666666em;" class="mspace"></span><span class="mord"><span class="mopen nulldelimiter"></span><span class="mfrac"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height:1.2603300000000002em;" class="vlist"><span style="top:-2.314em;"><span style="height:3em;" class="pstrut"></span><span class="mord"><span class="mord"><span class="mord mathdefault">x</span><span class="msupsub"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height:0.311664em;" class="vlist"><span style="top:-2.5500000000000003em;margin-left:0em;margin-right:0.05em;"><span style="height:2.7em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span style="margin-right:0.05724em;" class="mord mathdefault mtight">j</span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height:0.286108em;" class="vlist"><span></span></span></span></span></span></span><span style="margin-right:0.2222222222222222em;" class="mspace"></span><span class="mbin">−</span><span style="margin-right:0.2222222222222222em;" class="mspace"></span><span class="mord"><span class="mord mathdefault">x</span><span class="msupsub"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height:0.33610799999999996em;" class="vlist"><span style="top:-2.5500000000000003em;margin-left:0em;margin-right:0.05em;"><span style="height:2.7em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span style="margin-right:0.03148em;" class="mord mathdefault mtight">k</span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height:0.15em;" class="vlist"><span></span></span></span></span></span></span></span></span><span style="top:-3.23em;"><span style="height:3em;" class="pstrut"></span><span style="border-bottom-width:0.04em;" class="frac-line"></span></span><span style="top:-3.677em;"><span style="height:3em;" class="pstrut"></span><span class="mord"><span class="mord mathdefault">x</span><span style="margin-right:0.2222222222222222em;" class="mspace"></span><span class="mbin">−</span><span style="margin-right:0.2222222222222222em;" class="mspace"></span><span class="mord"><span class="mord mathdefault">x</span><span class="msupsub"><span class="vlist-t vlist-t2"><span class="vlist-r"><span style="height:0.33610799999999996em;" class="vlist"><span style="top:-2.5500000000000003em;margin-left:0em;margin-right:0.05em;"><span style="height:2.7em;" class="pstrut"></span><span class="sizing reset-size6 size3 mtight"><span style="margin-right:0.03148em;" class="mord mathdefault mtight">k</span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height:0.15em;" class="vlist"><span></span></span></span></span></span></span></span></span></span><span class="vlist-s">​</span></span><span class="vlist-r"><span style="height:0.972108em;" class="vlist"><span></span></span></span></span></span><span class="mclose nulldelimiter"></span></span></span></span></span></span></span>
<ol>
<li><div class="pe-task-2 d-flex"><jhi-programming-exercise-instructions-task-status _nghost-quc-c226="" class="ng-star-inserted"><div _ngcontent-quc-c226="" class="guided-tour">
    <!---->
    <!---->
    <fa-icon _ngcontent-quc-c226="" size="lg" class="ng-fa-icon test-icon text-secondary ng-star-inserted"><svg role="img" aria-hidden="true" focusable="false" data-prefix="fas" data-icon="circle-question" class="svg-inline--fa fa-circle-question fa-lg" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path fill="currentColor" d="M256 512c141.4 0 256-114.6 256-256S397.4 0 256 0S0 114.6 0 256S114.6 512 256 512zM169.8 165.3c7.9-22.3 29.1-37.3 52.8-37.3h58.3c34.9 0 63.1 28.3 63.1 63.1c0 22.6-12.1 43.5-31.7 54.8L280 264.4c-.2 13-10.9 23.6-24 23.6c-13.3 0-24-10.7-24-24V250.5c0-8.6 4.6-16.5 12.1-20.8l44.3-25.4c4.7-2.7 7.6-7.7 7.6-13.1c0-8.4-6.8-15.1-15.1-15.1H222.6c-3.4 0-6.4 2.1-7.5 5.3l-.4 1.2c-4.4 12.5-18.2 19-30.6 14.6s-19-18.2-14.6-30.6l.4-1.2zM288 352c0 17.7-14.3 32-32 32s-32-14.3-32-32s14.3-32 32-32s32 14.3 32 32z"></path></svg></fa-icon><!---->
    <span _ngcontent-quc-c226="" class="task-name ng-star-inserted">lagrange</span><!---->
    <span _ngcontent-quc-c226="" class="guided-tour test-status--linked text-secondary ng-star-inserted">0 of 1 tests passing</span><!---->
    
    <!---->
</div>
</jhi-programming-exercise-instructions-task-status></div>Implement the function <code>lagrange : (float * float) list -&gt; float -&gt; float</code> that returns the interpolated polynomial <span><span class="katex"><span class="katex-mathml"><math><mrow><mi>L</mi></mrow>L</math></span><span aria-hidden="true" class="katex-html"><span class="base"><span style="height: 0.68333em; vertical-align: 0em;" class="strut"></span><span class="mord mathdefault">L</span></span></span></span></span>.</li>
</ol>
<p><em>Note: This is not a team exercise, you must submit your individual solution.</em>  </p>
<p><strong>Tests</strong></p>
<pre><code class="hljs">utop <span class="hljs-meta"># lagrange [100.,231.; 200.,12.; 300.,382.5] 0.3;;</span>
- : <span class="hljs-type">float</span> = <span class="hljs-number">1036.19290275</span>
</code></pre></div>