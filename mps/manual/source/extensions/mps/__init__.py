'''
Sphinx extensions for the MPS documentation.
See <http://sphinx-doc.org/extensions.html>
'''

from collections import defaultdict
from inspect import isabstract, isclass
import re
import designs

from docutils import nodes, transforms
from sphinx import addnodes
from sphinx.directives.other import VersionChange
from sphinx.domains import Domain
from sphinx.roles import XRefRole
from sphinx.util.compat import Directive, make_admonition
from sphinx.util.nodes import set_source_info, process_index_entry
from sphinx.locale import versionlabels
versionlabels['deprecatedstarting'] = 'Deprecated starting with version %s'

class MpsDomain(Domain):
    label = 'MPS'
    name = 'mps'

class MpsDirective(Directive):
    @classmethod
    def add_to_app(cls, app):
        if hasattr(cls, 'name'): name = cls.name
        elif hasattr(cls, 'nodecls'): name = cls.nodecls.__name__
        else: return
        if hasattr(cls, 'nodecls') and hasattr(cls, 'visit'):
            app.add_node(cls.nodecls, html = cls.visit, latex = cls.visit,
                         text = cls.visit, man = cls.visit)
        if hasattr(cls, 'domain'):
            app.add_directive_to_domain(cls.domain, name, cls)
        else:
            app.add_directive(name, cls)

class MpsPrefixDirective(MpsDirective):
    domain = 'mps'
    name = 'prefix'
    has_content = True

    def run(self):
        targetid = self.content[0]
        self.state.document.mps_tag_prefix = targetid
        targetnode = nodes.target('', '', ids=[targetid])
        return [targetnode]

def mps_tag_role(name, rawtext, text, lineno, inliner, options={}, content=[]):

    try:
        targetid = '.'.join([inliner.document.mps_tag_prefix, text])
    except AttributeError:
        return [], [inliner.document.reporter.warning
                ('mps:tag without mps:prefix', line=lineno)]
    if len(text) == 0:
        return [], [inliner.document.reporter.error
                ('missing argument for mps:tag', line=lineno)]
    targetnode = nodes.target('', '', ids=[targetid])
    tag = '.{}:'.format(text)
    refnode = nodes.reference('', '', refid=targetid, classes=['mpstag'],
                              *[nodes.Text(tag)])
    return [targetnode, refnode], []

def mps_ref_role(name, rawtext, text, lineno, inliner, options={}, content=[]):
    textnode = nodes.Text(text)
    if text.startswith('.'):
        # Tag is relative to current prefix and so points elsewhere
        # in this document, so create reference node.
        try:
            targetid = inliner.document.mps_tag_prefix + text
            refnode = nodes.reference('', '', refid=targetid, *[textnode])
        except AttributeError:
            return [textnode], [inliner.document.reporter.warning
                                (':mps:ref without mps:prefix', line=lineno)]
    else:
        # Tag is absolute: need to create pending_xref node.
        refnode = addnodes.pending_xref('', refdomain='std', reftarget=text,
                                        reftype='view')
        refnode += textnode
    return [refnode], []

class Admonition(nodes.Admonition, nodes.Element):
    pass

def visit_admonition_node(self, node):
    self.visit_admonition(node)

def depart_admonition_node(self, node):
    self.depart_admonition(node)

class AdmonitionDirective(MpsDirective):
    label = 'Admonition'
    has_content = True
    visit = visit_admonition_node, depart_admonition_node

    @classmethod
    def add_to_app(cls, app):
        if not hasattr(cls, 'nodecls'): return
        super(AdmonitionDirective, cls).add_to_app(app)

    def run(self):
        ad = make_admonition(self.nodecls, self.name, [self.label],
                             self.options, self.content, self.lineno,
                             self.content_offset, self.block_text,
                             self.state, self.state_machine)
        return ad

class PluralDirective(AdmonitionDirective):
    def run(self):
        ad = super(PluralDirective, self).run()
        refs = sum(1 for node in ad[0][1]
                   if isinstance(node, addnodes.pending_xref)
                   or isinstance(node, nodes.Referential))
        if refs > 1:
            assert(isinstance(ad[0][0], nodes.title))
            ad[0][0][0] = nodes.Text(self.plural)
        return ad

class aka(Admonition):
    pass

class AkaDirective(AdmonitionDirective):
    nodecls = aka
    label = 'Also known as'

class bibref(Admonition):
    pass

class BibrefDirective(PluralDirective):
    nodecls = bibref
    label = 'Related publication'
    plural = 'Related publications'

class deprecated(Admonition):
    pass

class DeprecatedDirective(AdmonitionDirective):
    nodecls = deprecated
    label = 'Deprecated'

class historical(Admonition):
    pass

class HistoricalDirective(AdmonitionDirective):
    nodecls = historical
    label = 'Historical note'

class link(Admonition):
    pass

class LinkDirective(PluralDirective):
    nodecls = link
    label = 'Related link'
    plural = 'Related links'

class note(Admonition):
    pass

class NoteDirective(AdmonitionDirective):
    nodecls = note
    label = 'Note'
    plural = 'Notes'

    def run(self):
        ad = super(NoteDirective, self).run()
        assert(isinstance(ad[0][0], nodes.title))
        if len(ad[0]) == 1: return ad
        if (isinstance(ad[0][1], nodes.enumerated_list)
            and sum(1 for _ in ad[0][1].traverse(nodes.list_item)) > 1
            or isinstance(ad[0][1], nodes.footnote)
            and sum(1 for _ in ad[0].traverse(nodes.footnote)) > 1):
            ad[0][0][0] = nodes.Text(self.plural)
        return ad

class opposite(Admonition):
    pass

class OppositeDirective(PluralDirective):
    nodecls = opposite
    label = 'Opposite term'
    plural = 'Opposite terms'

class relevance(Admonition):
    pass

class RelevanceDirective(AdmonitionDirective):
    nodecls = relevance
    label = 'Relevance to memory management'

class see(Admonition):
    pass

class SeeDirective(AdmonitionDirective):
    nodecls = see
    label = 'See'

class similar(Admonition):
    pass

class SimilarDirective(PluralDirective):
    nodecls = similar
    label = 'Similar term'
    plural = 'Similar terms'

class specific(Admonition):
    pass

class SpecificDirective(AdmonitionDirective):
    domain = 'mps'
    nodecls = specific
    label = 'In the MPS'

class topics(Admonition):
    pass

class TopicsDirective(PluralDirective):
    nodecls = topics
    label = 'Topic'
    plural = 'Topics'

class GlossaryTransform(transforms.Transform):
    """
    Make transformations to a document that affect the way it refers
    to glossary entries:

    1. Change parenthesized sense numbers (1), (2) etc. to
       superscripts in glossary entries and cross-references to
       glossary entries.

    2. Record glossary entries that consist only of "See: XREF" so
       that cross-references to them can be reported by
       `warn_indirect_terms` once the build is completed.

    3. Add "term" cross-reference targets for plurals.
    """

    # These are shared between all instances of the class.
    see_only_ids = set()
    xref_ids = defaultdict(list)
    default_priority = 999
    sense_re = re.compile(r'(.*)\s+(\([0-9]+\))$', re.S)

    def superscript_children(self, target):
        """
        Yield the children of `target` in order, removing
        parenthesized sense numbers like "(1)" and "(2)" from text
        nodes, and adding new superscript nodes as necessary.
        """
        for e in target:
            if not isinstance(e, nodes.Text):
                yield e
                continue
            m = self.sense_re.match(e)
            if not m:
                yield e
                continue
            yield nodes.Text(m.group(1))
            yield nodes.superscript(text = m.group(2))

    def apply(self):
        # Change parenthesized sense numbers to superscripts in
        # glossary entries.
        for target in self.document.traverse(nodes.term):
            target[:] = list(self.superscript_children(target))

        # Change parenthesized sense numbers to superscripts in
        # cross-references to glossary entries.
        for target in self.document.traverse(addnodes.pending_xref):
            if target['reftype'] == 'term':
                ids = self.xref_ids['term-{}'.format(target['reftarget'])]
                ids.append((target.source, target.line))
                if len(target) == 1 and isinstance(target[0], nodes.emphasis):
                    target[0][:] = list(self.superscript_children(target[0]))

        # Record glossary entries consisting only of "See: XREF".
        for target in self.document.traverse(nodes.definition_list_item):
            ids = set()
            for c in target:
                if isinstance(c, nodes.term):
                    ids = set(c['ids'])
                if (isinstance(c, nodes.definition)
                    and len(c) == 1
                    and isinstance(c[0], see)):
                    self.see_only_ids |= ids

        # Add cross-reference targets for plurals.
        objects = self.document.settings.env.domaindata['std']['objects']
        endings = [(l, l + 's') for l in 'abcedfghijklmnopqrtuvwxz']
        endings.extend([
            ('ss', 'sses'),
            ('ing', 'ed'),
            ('y', 'ies'),
            ('e', 'ed'),
            ('', 'ed'),
            ])
        for (name, fullname), value in objects.items():
            if name != 'term':
                continue
            m = self.sense_re.match(fullname)
            if m:
                old_fullname = m.group(1)
                sense = ' ' + m.group(2)
            else:
                old_fullname = fullname
                sense = ''
            if any(old_fullname.endswith(e) for _, e in endings):
                continue
            for old_ending, new_ending in endings:
                if not old_fullname.endswith(old_ending):
                    continue
                new_fullname = '{}{}{}'.format(old_fullname[:len(old_fullname) - len(old_ending)], new_ending, sense)
                new_key = name, new_fullname
                if new_key not in objects:
                    objects[new_key] = value

    @classmethod
    def warn_indirect_terms(cls, app, exception):
        """
        Output a warning for each cross-reference to a term that
        consists only of a "see:" paragraph. These cross-references
        should be changed in the source text to refer to the target of
        the "see:".
        """
        if not exception:
            for i in cls.see_only_ids:
                for doc, line in cls.xref_ids[i]:
                    print('{}:{}: WARNING: cross-reference to {}.'
                          .format(doc, line, i))

        

def setup(app):
    designs.convert_updated(app)
    app.add_domain(MpsDomain)
    app.add_role_to_domain('mps', 'tag', mps_tag_role)
    app.add_role_to_domain('mps', 'ref', mps_ref_role)
    app.add_transform(GlossaryTransform)
    app.connect('build-finished', GlossaryTransform.warn_indirect_terms)
    for g in globals().itervalues():
        if isclass(g) and issubclass(g, MpsDirective):
            g.add_to_app(app)
