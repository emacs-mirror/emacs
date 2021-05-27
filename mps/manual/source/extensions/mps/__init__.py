'''
Sphinx extensions for the MPS documentation.
See <http://sphinx-doc.org/extensions.html>
'''

from collections import defaultdict
from inspect import isabstract, isclass
import re
import warnings

from docutils import nodes, transforms
from docutils.parsers.rst import Directive
from docutils.parsers.rst.directives.admonitions import BaseAdmonition
from sphinx import addnodes
from sphinx.directives.other import VersionChange
from sphinx.domains import Domain
from sphinx.domains.changeset import versionlabels
from sphinx.locale import admonitionlabels
from sphinx.roles import XRefRole
from sphinx.util.nodes import set_source_info, process_index_entry

from . import designs

versionlabels['deprecatedstarting'] = "Deprecated starting with version %s"
admonitionlabels.update(
    aka="Also known as",
    bibref="Related publication",
    bibrefs="Related publications",
    deprecated="Deprecated",
    historical="Historical note",
    link="Related link",
    links="Related links",
    note="Note",
    notes="Notes",
    opposite="Opposite term",
    opposites="Opposite terms",
    relevance="Relevance to memory management",
    see="See",
    similar="Similar term",
    similars="Similar terms",
    specific="In the MPS",
    topics="Topic",
    topicss="Topics"),

class MpsDomain(Domain):
    label = 'MPS'
    name = 'mps'

class MpsDirective(Directive):
    @classmethod
    def add_to_app(cls, app):
        if hasattr(cls, 'name'):
            name = cls.name
        elif hasattr(cls, 'node_class') and cls.node_class is not None:
            name = cls.node_class.__name__
        else:
            return
        if hasattr(cls, 'node_class') and hasattr(cls, 'visit'):
            app.add_node(cls.node_class, html=cls.visit, latex=cls.visit,
                         text=cls.visit, man=cls.visit)
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
    plural = False

def visit_admonition_node(self, node):
    name = type(node).__name__ + ('s' if node.plural else '')
    self.visit_admonition(node, name=name)

def depart_admonition_node(self, node):
    self.depart_admonition(node)

class AdmonitionDirective(MpsDirective, BaseAdmonition):
    has_content = True
    visit = visit_admonition_node, depart_admonition_node

class PluralDirective(AdmonitionDirective):
    def run(self):
        ad = super(PluralDirective, self).run()
        refs = sum(1 for node in ad[0][0]
                   if isinstance(node, (addnodes.pending_xref,
                                        nodes.Referential)))
        if refs > 1:
            ad[0].plural = True
        return ad

class aka(Admonition):
    pass

class AkaDirective(AdmonitionDirective):
    node_class = aka

class bibref(Admonition):
    pass

class BibrefDirective(PluralDirective):
    node_class = bibref

class deprecated(Admonition):
    pass

class DeprecatedDirective(AdmonitionDirective):
    node_class = deprecated

class historical(Admonition):
    pass

class HistoricalDirective(AdmonitionDirective):
    node_class = historical

class link(Admonition):
    pass

class LinkDirective(PluralDirective):
    node_class = link

class note(Admonition):
    pass

class NoteDirective(AdmonitionDirective):
    node_class = note

    def run(self):
        ad = super(NoteDirective, self).run()
        if (isinstance(ad[0][0], nodes.enumerated_list)
            and sum(1 for _ in ad[0][0].traverse(nodes.list_item)) > 1
            or isinstance(ad[0][0], nodes.footnote)
            and sum(1 for _ in ad[0].traverse(nodes.footnote)) > 1):
            ad[0].plural = True
        return ad

class opposite(Admonition):
    pass

class OppositeDirective(PluralDirective):
    node_class = opposite

class relevance(Admonition):
    pass

class RelevanceDirective(AdmonitionDirective):
    node_class = relevance

class see(Admonition):
    pass

class SeeDirective(AdmonitionDirective):
    node_class = see

class similar(Admonition):
    pass

class SimilarDirective(PluralDirective):
    node_class = similar

class specific(Admonition):
    pass

class SpecificDirective(AdmonitionDirective):
    domain = 'mps'
    node_class = specific

class topics(Admonition):
    pass

class TopicsDirective(PluralDirective):
    node_class = topics

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

        # Add cross-reference targets for plural and capitalized forms.
        objects = self.document.settings.env.domaindata['std']['objects']
        regular = 'abcedfghijklmnopqrtuvwxz'
        endings = [(l, l + 's') for l in regular + regular.upper()]
        endings.extend([
            ('ss', 'sses'),
            ('ing', 'ed'),
            ('y', 'ies'),
            ('e', 'ed'),
            ('', 'ed'),
            ])
        for (name, fullname), value in list(objects.items()):
            if name != 'term':
                continue
            m = self.sense_re.match(fullname)
            if m:
                old_fullname = m.group(1)
                sense = ' ' + m.group(2)
            else:
                old_fullname = fullname
                sense = ''

            def maybe_add(new_fullname):
                new_key = name, new_fullname
                if new_key not in objects:
                    objects[new_key] = value

            maybe_add(old_fullname.capitalize())
            if any(old_fullname.endswith(e) for _, e in endings):
                continue
            for old_ending, new_ending in endings:
                if not old_fullname.endswith(old_ending):
                    continue
                new_fullname = '{}{}{}'.format(old_fullname[:len(old_fullname) - len(old_ending)], new_ending, sense)
                maybe_add(new_fullname)
                maybe_add(new_fullname.capitalize())

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
    for g in globals().values():
        if isclass(g) and issubclass(g, MpsDirective):
            g.add_to_app(app)
