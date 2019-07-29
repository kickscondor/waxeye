; Qaldron library for Waxeye
;
; Licensed under the Blue Oak license. See 'LICENSE.md' for details.

edge-trans: string or number or range

edge: type [
  trans: edge-trans or list (edge-trans)
  state: 0
  voided: false
]

state: type [
  edges: list (edge)
  match: false
]

fa: type [
  type: string
  states: list (states)
  mode: string
]

parse-error: type [
  pos: number
  line: number
  col: number
  nt: number
]

parse-error/to-string: block [
  "parse error: failed to match '$nt' at line=$line, col=$col, pos=$pos\n"
]

ast: type [
  type: string
  children: list (ast or string)
  pos: number
]

parser: type [
  start: number
  eof: flag
  automata: list (fa)
]

parser/parse: block [input: string.
  var input-len: input/length,
    input-pos: 0,
    line: 1,
    column: 0,
    last-cr: false,
    error-pos: 0,
    error-line: 1,
    error-col: 0,
    error-nt: automata[start]/type,
    fa-stack: [],
    cache: table ()

  var match-automaton: block [index.
    var start-pos: input-pos,
      key: [index, start-pos]

    if (cache/$key) [
      var cached-item: cache/$key
      (input-pos, line, column, last-cr): cached-item/1..4
      return cached-item/0
    ]

    var start-line: line,
      start-col: column,
      start-cr: last-cr,
      automaton: automata/$index,
      type: automaton/type,
      mode: automaton/mode

    fa-stack/push (automaton)
    var res: match-state (0)
    fa-stack/pop ()

    var value:
      if (type = '*and') [
        input-pos: start-pos, line: start-line, column: start-col, last-cr: start-cr
        res
      ] else if (type = '*not') [
        input-pos: start-pos, line: start-line, column: start-col, last-cr: start-cr
        if (res) [
          update-error ()
        ] else [
          true
        ]
      ] else [
        if (res) [
          if (mode = 'void') [
            true
          ] else if (mode = 'prune') [
            if (res/length = 0)      [ true ]
            else if (res/length = 1) [ res/0 ]
            else [ ast (type, res, [start-pos, input-pos]) ]
          ] else [
            ast (type, res, [start-pos, input-pos])
          ]
        ] else [
          update-error ()
        ]
      ]

    cache/$key: (value, input-pos, line, column, last-cr)
    value
  ]

  var match-state: block [index.
    var state: fa-stack/-1/states/$index
    var res: match-edges (state/edges)
    if (res) [res] else [state.match and []]
  ]

  var match-edges: block [edges.
    if (edges = []) [
      false
    ] else [
      var res: match-edge (edges/0)
      if (res) [res] else [match-edges (edges/1..-1)]
    ]
  ]

  var match-edge: block [edge.
    var start-pos: input-pos,
      start-line: line,
      start-col: column,
      start-cr: last-cr,
      t: edge/trans
      res:
        if (t = '*wild') [
          if (input-pos < input-len) [mv()]
          else [update-error()]
        ] else if (t is string) [
          if (input-pos < input-len and t/char-at (0) = input/char-at(input-pos)) [mv()]
          else [update-error()]
        ] else if (t is list) [
          if (input-pos < input-len and within-set (t, input/char-at(input-pos))) [mv()]
          else [update-error()]
        ] else if (t is number) [
          match-automaton (t)
        ] else [
          false
        ]

    if (res) [
      var tran-res: match-state (edge/state)
      if (tran-res) [
        if (edge/voided or res = true) [
          tran-res
        ] else [
          [res] + tran-res
        ]
      ] else [
        input-pos: start-pos, line: start-line, column: start-col, last-cr: start-cr
        false
      ]
    ] else [
      false
    ]
  ]

  var update-error: block [
    if (error-pos < input-pos) [
      error-pos: input-pos
      error-line: line
      error-col: column
      error-nt: fa-stack/-1/type
    ]
    false
  ]

  var mv: block [
    var ch: input/char-at(input-pos)
    input-pos: input-pos + 1
    if (ch = '\r') [
      line: line + 1
      column: 0
      last-cr: true
    ] else [
      if (ch = '\n') [
        if (not last-cr) [
          line: line + 1
          column: 0
        ]
      ] else [
        column: column + 1
      ]
      last-cr: false
    ]
    ch
  ]

  var eof-check: block [res.
    if (res) [
      if (eof-check and input-pos < input-len) [
        error (error-pos, error-line, error-col, error-nt)
      ] else [
        res
      ]
    ] else [
      error (error-pos, error-line, error-col, error-nt)
    ]
  ]

  var within-set: block [set, c.
    if (set = []) [
      false
    ] else [
      var aa: set/0
      if (aa is string) [
        if (aa/0 = c) [
          true
        ] else if (aa/0 < c) [
          within-set (set/1..-1, c)
        ] else [
          false
        ]
      ] else [
        if (aa/includes (c)) [
          true
        ] else if (aa/max () < c) [
          within-set (set/1..-1, c)
        ] else [
          false
        ]
      ]
    ]
  ]

  eof-check (match-automaton (start))
]
