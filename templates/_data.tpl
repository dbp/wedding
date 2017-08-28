<bind tag="class">nopic</bind>
<apply template="_base">

  <h4 style="text-align: center;">Attending: <attending-adults/> Adults, <attending-kids/> Kids (<friday-count/> Total Friday)</h4>
  <table cellspacing="0">
    <tr>
      <th>Confirmed</th>
      <th>URL</th>
      <th>People</th>
      <th>Friday/Saturday</th>
      <th>Food</th>
    </tr>
    <rsvps>
      <tr>
        <td>
          <confirmed>✔</confirmed>
        </td>
        <td>
          <a href="/rsvp?k=${k}"><k/></a>
        </td>
        <td> <people>
          <include><strong><name/></strong></include>
          <not-include><name/></not-include>
          <span style="display: none">
            <not-kid><a href="/data/person_is_kid?i=${id}&s=${s}">NK</a></not-kid>
            <is-kid><a href="/data/person_not_kid?i=${id}&s=${s}">K</a></is-kid>
            <locked><a href="/data/person_unlock?i=${id}&s=${s}">✎</a></locked>
            <not-locked>
              <a href="/data/person_lock?i=${id}&s=${s}">🔒</a>
              <a href="/data/person_delete?i=${id}&s=${s}" onclick="return confirm('Are you sure you want to remove ${name}?')">✗</a>
            </not-locked>
          </span>, 
        </people>
        <span style="display: none">
          <a href="/data/person_add?i=${id}&s=${s}">Add</a>
        </span>
        </td>
        <td>
          <friday-checked>F,</friday-checked><saturday-checked>S</saturday-checked>
        </td>
        <td>
          <food/>
        </td>
      </tr>
    </rsvps>
  </table>


</apply>
