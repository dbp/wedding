<apply template="_base">

    <form action="rsvp_data_merge">
        <input type="hidden" name="s" value="crup"/>
        <input type="submit" value="MERGE"/>
    <table>
        <tr><th>MERGE</th>
            <th>Confirmed</th>
            <th>URL</th>
            <th>Lodging</th>
            <th>Days</th>
            <th>Food</th>
            <th>People</th>
        </tr>
        <rsvps>
            <tr>
                <td>
                    <input type="checkbox" name="merge" value="${id}"/>
                </td>
            <td>
                <confirmed>CONFIRMED</confirmed>
            </td>
            <td>
                <a href="/rsvp?k=${k}"><k/></a>
            </td>
            <td>
                <lodging/>
            </td>
            <td>
                <friday-checked>Friday</friday-checked>
                <saturday-checked>Saturday</saturday-checked>
            </td>
            <td><food/></td>
            <td> <people>
                <include><strong><name/></strong></include>
                    <not-include><name/></not-include>,
                </people>

            </td>
        </tr>
        </rsvps>
    </table>

    </form>

</apply>
