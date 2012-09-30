(define-skeleton textile-code-region
  "textile code block"
  nil
  "<pre><code>" _ "</code></pre>")

(define-skeleton textile-code-highlight
  "textile code highlight"
  nil
  "{% highlight %}" _ "{% endhighlight %}")

(define-skeleton org-insert-code-block
  "org-mode code block"
  nil
  "#+BEGIN_SRC" _ "#+END_SRC")




