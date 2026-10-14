package info.kwarc.probgen

/** the Unicode characters of LaTeX commands
  Used for the MathML (browser) rendering of names: the same name string, e.g. "\gamma", is also used for the sTeX output, so it is written in LaTeX and translated to Unicode only for MathML.
  
  The table is MMT's unicode-latex-map, copied into [[mmtMap]] from: https://github.com/UniFormal/MMT/blob/master/src/mmt-api/resources/unicode/unicode-latex-map

  Important: Names are also written into the sTeX output, so only real LaTeX commands may be used in them, e.g. "\rightarrow" rather than MMT's shorthand "\ra".
  */
object Symbols {

  def unicode(command: String): Option[String] = table.get(command)

  /** replaces every LaTeX command in s that we know, e.g. "\alpha\beta" -> "αβ" */
  def replaceCommands(s: String): String =
    Command.replaceAllIn(s, m => java.util.regex.Matcher.quoteReplacement(unicode(m.group(1)).getOrElse(m.matched)))

  /** whether the character names something (MathML <mi>) rather than being an operator, relation, arrow or bracket (MathML <mo>) */
  def isIdentifier(u: String): Boolean =
    u.nonEmpty && (Character.isLetter(u.codePointAt(0)) || identifierSymbols.contains(u))

  private val Command = "\\\\([A-Za-z]+)".r

  /** symbols that are not letters in Unicode but name a value */
  private val identifierSymbols = Set("∅", "∞", "∂", "∇", "℘")

  private lazy val table: Map[String, String] =
    mmtMap.linesIterator.map(_.trim)
      .filter(l => l.startsWith("j") && l.contains("|"))
      .map { l =>
        val bar = l.indexOf('|')
        l.substring(1, bar) -> l.substring(bar + 1)
      }.toMap

  private val mmtMap = """
jsharp|♯
jflat|♭
jnatural|♮
jrightarrow|→
jleftarrow|←
jleftrightarrow|↔
jRightarrow|⇒
jLeftarrow|⇐
jLeftrightarrow|⇔
jhookrightarrow|↪
jhookleftarrow|↩
jlongrightarrow|⟶
jlongleftarrow|⟵
jlongleftrightarrow|⟷
jLongrightarrow|⟹
jLongleftarrow|⟸
jLongleftrightarrow|⟺
jmapsto|↦
jlongmapsto|⟼
jrightsquigarrow|⇝
juparrow|↑
jdownarrow|↓
jupdownarrow|↕
jUparrow|⇑
jDownarrow|⇓
jUpdownarrow|⇕
jast|∗
// changed from MMT: ★ is \bigstar; \star is ⋆
jstar|⋆
jcdot|⋅
jcirc|∘
jbullet|∙
jtimes|×
jpm|±
jmp|∓
// changed from MMT: ∕ is a division slash; \div is ÷
jdiv|÷
jwedge|∧
jvee|∨
jcap|∩
jcup|∪
jsqcap|⊓
jsqcup|⊔
joplus|⊕
jominus|⊖
jotimes|⊗
joslash|⊘
jodot|⊙
juplus|⊎
jbigcirc|◯
jbigwedge|⋀
jbigvee|⋁
jbigcap|⋂
jbigcup|⋃
jbigsqcup|⨆
jbigoplus|⨁
jbigotimes|⨂
jbiguplus|⨄
jsubset|⊂
jsupset|⊃
jsubseteq|⊆
jsupseteq|⊇
jnsubseteq|⊈
jnsupseteq|⊉
jsubsetneq|⊊
jsupsetneq|⊋
jleq|≤
jgeq|≥
jll|≪
jgg|≫
jprec|≺
jsucc|≻
jpreceq|≼
jsucceq|≽
// changed from MMT: ≨ is \lneqq; \nleq is ≰
jnleq|≰
// changed from MMT: ≩ is \gneqq; \ngeq is ≱
jngeq|≱
jsqsubset|⊏
jsqsupset|⊐
// changed from MMT: these two were a second jsubseteq/jsupseteq, which overrode ⊆/⊇ above
jsqsubseteq|⊑
jsqsupseteq|⊒
jin|∈
jni|∋
jneq|≠
jdoteq|≐
jcirceq|≗
jdotplus|∔
jequiv|≡
jsim|∼
jsimeq|≃
jcong|≅
jncong|≇
japprox|≈
japproxeq|≊
jtriangleright|▷
jtriangleleft|◁
jblacktriangleright|▶
jblacktriangleleft|◀
jvdash|⊢
jdashv|⊣
jVdash|⊩
jVvdash|⊪
jmodels|⊧
jtop|⊤
jbot|⊥
jneg|¬
jforall|∀
jexists|∃
jnexists|∄
jsquare|□
jdiamond|⋄
jemptyset|∅
// changed from MMT: the Hebrew letter א; \aleph is the math symbol ℵ
jaleph|ℵ
// changed from MMT: the Hebrew letter ג; \gimel is the math symbol ℷ
jgimel|ℷ
jinfty|∞
jsum|∑
jprod|∏
jcoprod|∐
// changed from MMT: ∐ is \coprod; \amalg is ⨿
jamalg|⨿
jcomplement|∁
jsetminus|∖
jcolon|∶
jlangle|⟨
jrangle|⟩
jlbrace|{
jrbrace|}
jlceil|⌈
jrceil|⌉
jlfloor|⌊
jrfloor|⌋
julcorner|⌜
jurcorner|⌝
jllcorner|⌞
jlrcorner|⌟
jldots|…
jvdots|⋮
jGamma|Γ
jDelta|Δ
jTheta|Θ
jLambda|Λ
jXi|Ξ
jPi|Π
jSigma|Σ
jUpsilon|Υ
jPhi|Φ
jPsi|Ψ
jOmega|Ω
jalpha|α
jbeta|β
jgamma|γ
jdelta|δ
// This is the way LaTeX does the epsilons. Same for the phis and thetas below.
jepsilon|ϵ
jvarepsilon|ε
jzeta|ζ
jeta|η
jtheta|θ
jvartheta|ϑ
jiota|ι
jkappa|κ
jlambda|λ
jmu|μ
jnu|ν
jxi|ξ
jpi|π
jrho|ρ
jsigma|σ
jtau|τ
jupsilon|υ
jphi|ϕ
jvarphi|φ
jchi|χ
jpsi|ψ
jomega|ω
jnabla|∇
jpartial|∂
jdagger|†
jint|∫
joint|∮
// changed from MMT: ㏑ is a single compatibility character; \ln prints the letters "ln"
jln|ln
jperp|⟂
jparallel|∥
jnparallel|∦
jprime|′
"""
}
